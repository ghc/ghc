#!groovy

/*
  Jenkins dependencies:
   * Pipeline Utility steps plugin

  Linux (Debian) worker dependencies:
   * xutil-dev curl automake autoconf libtool python3 python3-sphinx, llvm-4.0

   Requires approvals for:
   * new net.sf.json.JSONObject

*/

import net.sf.json.JSONObject

properties(
  [
    parameters(
      [
        booleanParam(name: 'build_docs', defaultValue: false, description: 'build and upload documentation'),
        booleanParam(name: 'nightly', defaultValue: false, description: 'are we building a nightly?'),
        booleanParam(name: 'runNofib', defaultValue: false, description: 'run nofib and archive results')
      ])
  ])


stage("Build source distribution") {
  node(label: 'linux') {
    stage("Checking out tree") {
      checkout scm
      sh """
         git submodule update --init --recursive
         mk/get-win32-tarballs.sh fetch all
         """
    }
    stage("Configuring tree") {
      sh """
         ./boot
         ./configure
         """
    }
    stage("Build tarballs") {
      def version = getMakeValue('make', 'ProjectVersion')
      sh "make sdist"
      sh "mv sdistprep/ghc-${version}-src.tar.xz ghc-src.tar.xz"
      sh "mv sdistprep/ghc-${version}-testsuite.tar.xz ghc-testsuite.tar.xz"
      sh "mv sdistprep/ghc-${version}-windows-extra-src.tar.xz ghc-win32-tarballs.tar.xz"

      def json = new JSONObject()
      json.put('dirName', "ghc-${version}" as String)
      json.put('commit', resolveCommitSha('HEAD'))
      writeJSON(file: 'src-dist.json', json: json)

      def src_dist_files = 'ghc-src.tar.xz,ghc-win32-tarballs.tar.xz,src-dist.json'
      stash(name: 'source-dist', includes: src_dist_files)
      stash(name: 'testsuite-dist', includes: 'ghc-testsuite.tar.xz')
      archiveArtifacts artifacts: src_dist_files
      archiveArtifacts artifacts: 'ghc-testsuite.tar.xz'
    }
  }
}

parallel (
  "linux x86-64"       : {
    node(label: 'linux && amd64') {
      buildAndTestGhc(targetTriple: 'x86_64-linux-gnu')
      if (params.build_docs) {
        updateReadTheDocs()
        updateUsersGuide()
      }
    }
  },
  "linux x86-64 -> aarch64 unreg" : {
    node(label: 'linux && amd64') {buildAndTestGhc(crossCompiling: true, targetTriple: 'aarch64-linux-gnu', unreg: true)}
  },
  "linux x86-64 -> aarch64" : {
    node(label: 'linux && amd64') {buildGhc(crossCompiling: true, targetTriple: 'aarch64-linux-gnu')}
    node(label: 'linux && aarch64') {testGhc(targetTriple: 'aarch64-linux-gnu')}
  },
  "aarch64"            : {
    node(label: 'linux && aarch64') {buildGhc(targetTriple: 'aarch64-linux-gnu')}
  },
  "freebsd"            : {
    node(label: 'freebsd && amd64') {
      buildGhc(targetTriple: 'x86_64-portbld-freebsd11.0', makeCmd: 'gmake', disableLargeAddrSpace: true)
    }
  },
  // Requires cygpath plugin?
  "windows 64"         : {
    node(label: 'windows && amd64') {
      withMingw('MINGW64') { buildAndTestGhc(targetTriple: 'x86_64-w64-mingw32') }
    }
  },
  "windows 32"         : {
    node(label: 'windows && amd64') {
      withMingw('MINGW32') { buildAndTestGhc(targetTriple: 'x86_64-pc-msys') }
    }
  },
  /*
  "osx"                : {
    node(label: 'darwin') {buildGhc(targetTriple: 'x86_64-apple-darwin16.0.0')}
  }
  */
)

if (params.runNofib) {
  node(label: 'linux && amd64 && perf') {
    nofib(targetTriple: 'x86_64-linux-gnu')
  }
}


def withMingw(String msystem, Closure f) {
  // Derived from msys2's /etc/msystem
  String msysRoot = 'C:\\msys64'
  String carch, prefix, ghcPath
  home = sh(script: 'echo -n $HOME', returnStdout: true)
  if (msystem == 'MINGW32') {
    prefix = "${msysRoot}\\mingw32"
    carch = 'i686'
    ghcPath = "${home}/ghc-8.0.1-i386/bin"
  } else if (msystem == 'MINGW64') {
    prefix = "${msysRoot}\\mingw64"
    carch = 'x86_64'
    ghcPath = "${home}/ghc-8.0.2-x86_64/bin"
  } else {
    fail
  }
  String chost = "${carch}-w64-mingw32"

  withEnv(["MSYSTEM=${msystem}",
           "PATH+mingw=${prefix}\\bin",
           "PATH+ghc=${ghcPath}",
           "MSYSTEM_PREFIX=${prefix}",
           "MSYSTEM_CARCH=${carch}",
           "MSYSTEM_CHOST=${chost}",
           "MINGW_CHOST=${chost}",
           "MINGW_PREFIX=${prefix}",
           "MINGW_PACKAGE_PREFIX=mingw-w64-${carch}",
           "CONFIG_SITE=${prefix}/etc/config.site"
          ], f)
}

def installPackages(String[] pkgs) {
  sh "cabal install -j${env.THREADS} --with-compiler=`pwd`/inplace/bin/ghc-stage2 --package-db=`pwd`/inplace/lib/package.conf.d ${pkgs.join(' ')}"
}

def buildAndTestGhc(params) {
  buildGhc(params)
  testGhc(params)
}

def buildGhc(params) {
  String targetTriple = params?.targetTriple
  boolean crossCompiling = params?.crossCompiling ?: false
  boolean unreg = params?.unreg ?: false
  boolean disableLargeAddrSpace = params?.disableLargeAddrSpace ?: false
  String makeCmd = params?.makeCmd ?: 'make'

  withGhcSrcDist() {
    stage('Configure') {
      sh 'echo $PATH'
      sh "which ghc"

      def speed = 'NORMAL'
      if (params.nightly) {
        speed = 'SLOW'
      }
      build_mk = """
                Validating=YES
                ValidateSpeed=${speed}
                ValidateHpc=NO
                BUILD_DPH=NO
                """
      if (crossCompiling) {
        build_mk += """
                    # Cross compiling
                    HADDOCK_DOCS=NO
                    BUILD_SPHINX_HTML=NO
                    BUILD_SPHINX_PDF=NO
                    INTEGER_LIBRARY=integer-simple
                    WITH_TERMINFO=NO
                    """
      }
      writeFile(file: 'mk/build.mk', text: build_mk)

      def configure_opts = []
      if (crossCompiling) {
        configure_opts += '--target=${targetTriple}'
      }
      if (disableLargeAddrSpace) {
        configure_opts += '--disable-large-address-space'
      }
      if (unreg) {
        configure_opts += '--enable-unregisterised'
      }
      sh "./configure ${configure_opts.join(' ')}"
    }

    stage('Build') {
      sh "${makeCmd} -j${env.THREADS}"
    }

    stage('Prepare binary distribution') {
      sh "${makeCmd} binary-dist"
      def json = new JSONObject()
      def tarPath = getMakeValue(makeCmd, 'BIN_DIST_PREP_TAR_COMP')
      def tarName = sh(script: "basename ${tarPath}", returnStdout: true).trim()
      json.put('tarName', tarName)
      json.put('dirName', getMakeValue(makeCmd, 'BIN_DIST_NAME'))
      json.put('ghcVersion', getMakeValue(makeCmd, 'ProjectVersion'))
      json.put('targetPlatform', getMakeValue(makeCmd, 'TARGETPLATFORM'))
      echo "${json}"
      writeJSON(file: 'bindist.json', json: json)
      // Write a file so we can easily file the tarball and bindist directory later
      stash(name: "bindist-${targetTriple}", includes: "bindist.json,${tarName}")
      archiveArtifacts artifacts: tarName
    }
  }
}

String getMakeValue(String makeCmd, String value) {
  return sh(script: "${makeCmd} -s echo! VALUE=${value}", returnStdout: true)
}

def withTempDir(String name, Closure f) {
  sh """
     rm -Rf ${name} || true
     mkdir ${name}
     """
  try {
    dir(name) {
      f()
    }
  } finally {
    sh "rm -Rf ${name}"
  }
}

def withGhcSrcDist(Closure f) {
  withTempDir('src-dist') {
    stage('Unpack source distribution') {
      unstash(name: "source-dist")
      sh 'tar -xf ghc-src.tar.xz'
      sh 'tar -xf ghc-win32-tarballs.tar.xz'
    }

    def metadata = readJSON file: 'src-dist.json'
    dir(metadata.dirName) {
      f()
    }
  }
}

def withGhcBinDist(String targetTriple, Closure f) {
  withTempDir('bin-dist') {
    unstash "bindist-${targetTriple}"
    unstash "testsuite-dist"
    def metadata = readJSON file: "bindist.json"
    sh "tar -xf ${metadata.tarName}"
    sh "tar -xf ghc-testsuite.tar.xz"
    try {
      dir(metadata.dirName) {
        f()
      }
    } finally {
      sh "rm -R ${metadata.dirName}"
    }
  }
}

def testGhc(params) {
  String targetTriple = params?.targetTriple
  // See Note [Spaces in TEST_HC]
  String makeCmd = params?.makeCmd ?: 'make'
  String instDir="${pwd()}/bindisttest/install   dir"
  String testGhc="${instDir}/bin/ghc"

  withGhcBinDist(targetTriple) {
    stage('Configure') {
      if (isUnix()) {
          sh "./configure --prefix=\"${instDir}\""
          sh "${makeCmd} install"
      } else {
          sh "mkdir -p \"${instDir}\""
          sh "cp -a * \"${instDir}\""
      }
    }

    stage('Install testsuite dependencies') {
      if (params.nightly) {
        def pkgs = ['mtl', 'parallel', 'parsec', 'primitive', 'QuickCheck',
                    'random', 'regex-compat', 'syb', 'stm', 'utf8-string',
                    'vector']
        installPkgs pkgs
      }
    }

    stage('Run testsuite') {
      def target = 'test'
      if (params.nightly) {
        target = 'slowtest'
      }
      sh "${makeCmd} -Ctestsuite/tests LOCAL=0 BINDIST=YES THREADS=${env.THREADS} TEST_HC=\"${testGhc}\" ${target}"
      junit 'testsuite*.xml'
    }
  }
}

def nofib(params) {
  String targetTriple = params?.targetTriple
  String makeCmd = params?.makeCmd ?: 'make'
  withGhcBinDist(targetTriple) {
    stage('Run nofib') {
      installPkgs(['regex-compat'])
      sh """
        cd nofib
        ${makeCmd} clean
        ${makeCmd} boot
        ${makeCmd} >../nofib.log 2>&1
        """
      archiveArtifacts artifacts: 'nofib.log'
    }
  }
}

def resolveCommitSha(String ref) {
  return sh(script: "git rev-parse ${ref}", returnStdout: true).trim()
}

// Push update to ghc.readthedocs.org.
// Expects to be sitting in a build source tree.
def updateReadTheDocs() {
  git clone 'git@github.com:bgamari/ghc-users-guide'
  def commit = resolveCommitSha('HEAD')
  sh """
     export GHC_TREE=\$(pwd)
     cd ghc-users-guide
     ./export.sh
     git commit -a -m \"Update to ghc commit ${commit}\" || true
     git push
     """
}

// Push update to downloads.haskell.org/~ghc/master/doc.
// Expects to be sitting in a configured source tree.
def updateUsersGuide() {
  sh "${makeCmd} html haddock EXTRA_HADDOCK_OPTS=--hyperlinked-sources"
  sh '''
     out="$(mktemp -d)"
     mkdir -p $out/libraries

     cp -R docs/users_guide/build-html/users_guide $out/users-guide
     for d in libraries/*; do
         if [ ! -d $d/dist-install/doc ]; then continue; fi
         mkdir -p $out/libraries/$(basename $d)
         cp -R $d/dist-install/doc/*/* $out/libraries/\$(basename \$d)
     done
     cp -R libraries/*/dist-install/doc/* $out/libraries
     chmod -R ugo+r $out

     rsync -az $out/ downloads.haskell.org:public_html/master
     rm -R $out
     '''
}
