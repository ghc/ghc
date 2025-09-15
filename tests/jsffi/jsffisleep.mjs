export default async (__exports) => {
  __exports.testWouldBlock();
  await __exports.testLazySleep(1000000, 1024);
  await __exports.testThreadDelay(1000000, 1024);
  await __exports.testInterruptingSleep();
}
