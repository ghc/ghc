export default async (__exports) => {
  await __exports.testJSException();
  try {
    await __exports.testHSException();
  } catch (e) {
    console.log(e.message);
  }
};
