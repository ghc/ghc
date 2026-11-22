"""Give runnable jobs in other Pueue groups priority over GHC."""
import json
import os
import shutil
import subprocess
import time
from pathlib import Path

hold = Path(os.environ["XDG_RUNTIME_DIR"]) / "ghc-build-paused-by-priority"
pool = "ghcbuild.slice"
env = dict(os.environ)
env.pop("XDG_RUNTIME_DIR", None)
pueue = shutil.which("pueue")
if pueue is None:
    raise SystemExit("pueue is missing from PATH")

def queue(*args):
    return subprocess.check_output([pueue, *args], env=env, text=True, timeout=8)

def systemctl(*args):
    return subprocess.check_output(["systemctl", "--user", *args], text=True, timeout=8).strip()

def tag(value):
    return value if isinstance(value, str) else next(iter(value))

def other_work(s):
    for task in s["tasks"].values():
        if task["group"] == "ghc":
            continue
        status = tag(task["status"])
        if status == "Running":
            return True
        if status != "Queued" or s["groups"][task["group"]]["status"] != "Running":
            continue
        deps = [s["tasks"].get(str(k)) for k in task.get("dependencies", [])]
        if all(d is None or tag(d["status"]) == "Done" for d in deps):
            return True
    return False

def set_frozen(unit, freeze):
    active = systemctl("show", unit, "--property=ActiveState", "--value")
    if active != "active":
        return
    status = systemctl("show", unit, "--property=FreezerState", "--value")
    if status != ("frozen" if freeze else "running"):
        systemctl("freeze" if freeze else "thaw", unit)

while True:
    try:
        state = json.loads(queue("status", "--json"))
        if other_work(state):
            if not hold.exists():
                hold.write_text("resume" if state["groups"]["ghc"]["status"] == "Running" else "leave-paused")
            if state["groups"]["ghc"]["status"] == "Running":
                queue("pause", "--wait", "--group", "ghc")
            set_frozen(pool, True)
        elif hold.exists():
            set_frozen(pool, False)
            if hold.read_text() == "resume":
                queue("start", "--group", "ghc")
            hold.unlink()
    except (OSError, subprocess.SubprocessError, ValueError, KeyError) as error:
        print(str(error), flush=True)
    time.sleep(3)
