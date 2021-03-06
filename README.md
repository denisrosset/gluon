[![Build Status](https://api.travis-ci.org/denisrosset/gluon.png)](https://travis-ci.org/denisrosset/gluon/)

# Gluon - a library for Scala interoperability with dynamic languages

Gluon enables users to write Scala interface code in a robust manner.

The following Matlab code implements the sum of elements of a vector.

```matlab
function res = sumI(vec)
i = com.faacets.gluon.Interface.compile({
    'import com.faacets.gluon._'
    'Interface[Seq[Double], Double]("sumI", "seq") {'
    'seq => seq.sum'
    '}'
                   });
res = i.call(vec);
```

How to compile and pack the deliverables?

- Clone the repo.
- Run `sbt pack` to produce a copy of the Matlab example code and the required jars.
- The libraries and code are copied in `/target/pack`

To run the example code:

- Ensure that the Matlab Java Runtime Environment is at least version 1.8
  by running `version -java`. On Linux, the JRE can be changed by setting
  the MATLAB_JAVA environment variable; on Ubuntu with the Oracle JRE, use
  `export MATLAB_JAVA=/usr/lib/jvm/java-8-oracle/jre`
- Run Matlab, and change to the `/target/pack` directory
- Run `gluon_init` to load the JARs and warm-up the Scala compiler
  It takes generally a few seconds.
- Run `sumI([1 2 3 4])` to try the example code. The first call to
  any interface function requires compilation of Scala code, which
  takes a few hundred milliseconds. The compiled code is cached for
  subsequent calls.

