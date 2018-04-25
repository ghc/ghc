# Build flavours

Hadrian supports a few predefined _build flavours_, i.e. collections of build
settings that fully define a GHC build (see `src/Flavour.hs`). Users can add their
own build flavours if need be, as described
[here](https://github.com/snowleopard/hadrian/blob/master/doc/user-settings.md#build-flavour).

## Arguments

The following table summarises extra arguments passed to GHC in different build flavours.
There are four groups of arguments: arguments in `hsDefault` are passed to GHC for all Haskell
source files, `hsLibrary` arguments are added when compiling libraries, `hsCompiler`
when compiling the `compiler` library, and `hsGhc` when compiling/linking the GHC program.

<table>
  <tr>
    <th rowspan="3">Flavour</th>
    <th colspan="8">Extra arguments</th>
  </tr>
  <tr>
    <th colspan="2">hsDefault</td>
    <th colspan="2">hsLibrary</td>
    <th colspan="2">hsCompiler</td>
    <th colspan="2">hsGhc</td>
  </tr>
  <tr>
    <th>stage0</td>
    <th>stage1+</td>
    <th>stage0</td>
    <th>stage1+</td>
    <th>stage0</td>
    <th>stage1+</td>
    <th>stage0</td>
    <th>stage1+</td>
  </tr>
  <tr>
    <th>default<br></td>
    <td>-O<br>-H64m<br></td>
    <td>-O2<br>-H64m</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <th>quick</td>
    <td>-O0<br>-H64m</td>
    <td>-O0<br>-H64m</td>
    <td></td>
    <td>-O</td>
    <td>-O</td>
    <td></td>
    <td>-O</td>
    <td></td>
  </tr>
  <tr>
    <th>quickest</td>
    <td>-O0<br>-H64m</td>
    <td>-O0<br>-H64m</td>
    <td></td>
    <td></td>
    <td>-O</td>
    <td></td>
    <td>-O</td>
    <td></td>
  </tr>
  <tr>
    <th>perf</td>
    <td>-O<br>-H64m</td>
    <td>-O<br>-H64m</td>
    <td></td>
    <td>-O2</td>
    <td>-O</td>
    <td>-O2</td>
    <td>-O</td>
    <td>-O2</td>
  </tr>
  <tr>
    <th>prof</td>
    <td>-O0<br>-H64m</td>
    <td>-O0<br>-H64m</td>
    <td></td>
    <td>-O</td>
    <td>-O</td>
    <td>-O</td>
    <td>-O</td>
    <td>-O</td>
  </tr>
  <tr>
    <th>devel1</td>
    <td>-O<br>-H64m</td>
    <td>-O<br>-H64m</td>
    <td></td>
    <td>-dcore-lint</td>
    <td>-O0<br>-DDEBUG</td>
    <td></td>
    <td>-O0<br>-DDEBUG</td>
    <td></td>
  </tr>
  <tr>
    <th>devel2</td>
    <td>-O<br>-H64m</td>
    <td>-O<br>-H64m</td>
    <td></td>
    <td>-dcore-lint</td>
    <td></td>
    <td>-O0<br>-DDEBUG</td>
    <td></td>
    <td>-O0<br>-DDEBUG</td>
  </tr>
</table>

## Ways

Libraries and GHC can be built in different _ways_, e.g. with or without profiling
information. The following table lists ways that are built in different flavours.

<table>
    <tr>
        <th rowspan="2">Flavour</th>
        <th colspan="2">Library ways</th>
        <th colspan="2">RTS ways</th>
        <th colspan="2">Profiled GHC</th>
    </tr>
    <tr>
        <th>stage0</th>
        <th>stage1+</th>
        <th>stage0</th>
        <th>stage1+</th>
        <th>stage0</th>
        <th>stage1+</th>
    </tr>
    <tr>
    <th>default<br>perf<br>prof<br>devel1<br>devel2</td>
    <td>vanilla</td>
    <td>vanilla<br>profiling<br>dynamic</td>
    <td>logging<br>debug<br>threaded<br>threadedDebug<br>threadedLogging
        <br>debugDynamic<br>threadedDynamic<br>threadedDebugDynamic
        <br>loggingDynamic<br>threadedLoggingDynamic
    </td>
    <td>
        logging<br>debug<br>threaded<br>threadedDebug<br>
        threadedLogging<br>threadedProfiling
        <br>debugDynamic<br>threadedDynamic<br>threadedDebugDynamic
        <br>loggingDynamic<br>threadedLoggingDynamic
    </td>
    <td>Only in<br>prof<br>flavour</td>
    <td>Only in<br>prof<br>flavour</td>
</tr>
<tr>
    <th>quick</th>
    <td>vanilla</td>
    <td>vanilla<br>dynamic</td>
    <td>logging<br>debug<br>threaded<br>threadedDebug<br>threadedLogging
        <br>debugDynamic<br>threadedDynamic<br>threadedDebugDynamic
        <br>loggingDynamic<br>threadedLoggingDynamic
    </td>
    <td>logging<br>debug<br>threaded<br>threadedDebug<br>threadedLogging
        <br>debugDynamic<br>threadedDynamic<br>threadedDebugDynamic
        <br>loggingDynamic<br>threadedLoggingDynamic
    </td>
    <td>No</td>
    <td>No</td>
</tr>
<tr>
    <th>quickest</th>
    <td>vanilla</td>
    <td>vanilla</td>
    <td>vanilla<br>threaded</td>
    <td>vanilla<br>threaded</td>
    <td>No</td>
    <td>No</td>
</tr>
</table>
