function sleep(t) {
  return new Promise(res => setTimeout(res, t));
}

async function reallyGC() {
  gc(); // Only exposed by the --expose-gc V8 option
  await sleep(1024); // For some reason, this is needed for the FinalizationRegistry logic to actually work
}

export default async (__exports) => {
  __exports.testDynExportFree(114, 514, 1919810);

  const cont = await __exports.testDynExportGC(114, 514, 1919810);
  await reallyGC();
  await cont();
};
