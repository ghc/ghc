const gulp = require('gulp');
const uglify = require('gulp-uglify');
const browserify = require('browserify');
const source = require('vinyl-source-stream');
const buffer = require('vinyl-buffer');
const tsify = require('tsify');
const sourcemaps = require('gulp-sourcemaps');

function buildJS(targetFileName, files) {
  var b = browserify({ entries: files, debug: true });
  return b
    .plugin(tsify)
    .bundle()
    .pipe(source(targetFileName))
    .pipe(buffer())
    .pipe(sourcemaps.init({loadMaps: true}))
    .pipe(uglify().on('error', function(e) { console.log(e); }))
    .pipe(sourcemaps.write('.'))
    .pipe(gulp.dest('.'));
}

gulp.task('build-js', function(done) {
  buildJS('quick-jump.min.js', ['./js-src/quick-jump.tsx']);
  buildJS('haddock-bundle.min.js', ['./js-src/init.ts']);
  done();
});

gulp.task('default', gulp.series('build-js'));
