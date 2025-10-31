#!/usr/bin/env -S node

const puppeteer = require("puppeteer-core");
const fs = require("node:fs");
const path = require("node:path");

class Playground {
  static #token = Symbol("Playground");
  #browser;
  #page;

  static async create({ launchOpts, artifactDir }) {
    const playground = new Playground(Playground.#token);
    playground.#browser = await puppeteer.launch(launchOpts);

    playground.#page = await playground.#browser.newPage();
    await playground.#page.setRequestInterception(true);
    playground.#page.on("request", async (req) => {
      if (!req.url().startsWith("http://localhost")) {
        return req.continue();
      }

      try {
        const f = req.url().replace("http://localhost", artifactDir);
        return req.respond({
          status: 200,
          contentType:
            {
              ".html": "text/html",
              ".mjs": "application/javascript",
            }[path.extname(f)] || "application/octet-stream",
          body: await fs.promises.readFile(f),
        });
      } catch {
        return req.abort();
      }
    });

    await playground.#page.goto("http://localhost/index.html");
    await playground.#page.locator("#runBtn:enabled").wait();
    return playground;
  }

  async close() {
    await this.#browser.close();
  }

  async runMain({ mainSrc, ghcArgs }) {
    await Promise.all([
      this.#page.evaluate((mainSrc) => editor.setValue(mainSrc), mainSrc),
      this.#page.locator("#ghcArgs").fill(ghcArgs),
    ]);
    await this.#page.locator("#runBtn:enabled").click();
    await this.#page.locator("#runBtn:enabled").wait();

    const [stdout, stderr] = await Promise.all(
      ["#stdout", "#stderr"].map((el) =>
        this.#page
          .locator(el)
          .map((t) => t.value)
          .wait()
      )
    );

    return { stdout, stderr };
  }

  constructor(token) {
    if (token !== Playground.#token) {
      throw new Error("new Playground() is forbidden, use Playground.create()");
    }
  }
}

(async () => {
  const playground = await Playground.create({
    launchOpts: JSON.parse(process.argv[2]),
    artifactDir: process.cwd(),
  });

  try {
    const { stdout, stderr } = await playground.runMain({
      mainSrc: await fs.promises.readFile("./hello.hs", { encoding: "utf-8" }),
      ghcArgs: "-package ghc -v0",
    });
    process.stdout.write(stdout);
    process.stderr.write(stderr);
  } finally {
    await playground.close();
  }
})();
