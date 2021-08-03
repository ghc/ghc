
function output(x) {
  if(typeof document !== 'undefined') {
      document.write(x);
  } else if(typeof console !== 'undefined') {
      console.log(x);
  } else {
      print(x);
  }
}

function bufObj(n) {
  var b = new ArrayBuffer(n);
  return { buf: b
         , u8:  new Uint8Array(b)
         , i3:  new Int32Array(b)
	 };
}

function dumpData(r, n, key, blk1, blk2) {
  function dispArr(a) {
    var x = [];
    for(var i=0;i<32;i++) {
      var v = a[i];
      if(v < 10) x.push('  ' + v);
      else if(v < 100) x.push(' ' + v);
      else x.push('' + v);
    }
    return '[' + x.join(',') + ']';
  }
  output(r+':'+n+' key  ' + dispArr(key.u8));
  output(r+':'+n+' blk1 ' + dispArr(blk1.u8));
  output(r+':'+n+' blk2 ' + dispArr(blk2.u8));
}

var expected1 = [ 12,161,149,108,169, 43, 44,165
                , 54,238,255, 84, 12, 57,128, 73
                ,129,209,145,  8,175, 91,196,187
                ,206,146,  2, 51, 17, 15, 28,178]

var expected2 = [218,149,102, 61, 87,125,181,112
                , 57,152,185,201,198,  6,219, 19
                ,111,131,255,166, 33,252, 53, 27
                ,164, 46,117,219, 81,182,159, 48];

function test_threefish_process_block() {
  var start, end, i, key = bufObj(32), blk1 = bufObj(32), blk2 = bufObj(32);
  var benchIter = 4000000;
  var mb        = 64*benchIter/1000000;
  for(i=0;i<32;i++) {
    blk1.u8[i] = 1;
    key.u8[i]  = 128 + i;
  }
  dumpData(0, 0, key, blk1, blk2);
  for(i=1;i<=5;i++) {
    h$Threefish_256_Process_Block(key, blk1, blk2, false);
    dumpData(i, 1, key, blk1, blk2);
    h$Threefish_256_Process_Block(key, blk2, blk1, false);
    dumpData(i, 2, key, blk1, blk2);
  }
  for(i=0;i<32;i++) {
      if(blk1.u8[i] !== expected1[i] || blk2.u8[i] !== expected2[i])
        throw("unexpected value: " + i + " - " + blk1.u8[i] + ': ' + expected1[i] +
              "     " + blk2.u8[i] + ': ' + expected2[i]);
  }
  // benchmark
  // warmup
  for(i=0;i<50000;i++) {
    h$Threefish_256_Process_Block(key, blk1, blk2, false);
    h$Threefish_256_Process_Block(key, blk2, blk1, false);
  }
  start = Date.now();
  for(i=0;i<benchIter;i++) {
    h$Threefish_256_Process_Block(key, blk1, blk2, false);
    h$Threefish_256_Process_Block(key, blk2, blk1, false);
  }
  end = Date.now();
  output("processed " + mb + "MB in " + ((end-start)/1000) + "ms (" +
         (mb / ((end-start)/1000)) + "MB/s)");
               
}

test_threefish_process_block();
