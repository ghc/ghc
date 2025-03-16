export default async (__exports) => {
  const test = new Promise((res) => {
    const p = __exports.setTimeout(114514);
    p.throwTo("1919810");
    p.catch((err) => {
      console.log(`${err}`.split("\n")[0]);
      res();
    });
  });

  await test;
  process.exit();
};
