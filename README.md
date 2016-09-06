# Nyx

[![License Apache 2][badge-license]](LICENSE)

Master :
* [![Circle CI](https://circleci.com/gh/nlamirault/nyx/tree/master.svg?style=svg)](https://circleci.com/gh/nlamirault/nyx/tree/master)

Develop :
* [![Circle CI](https://circleci.com/gh/nlamirault/nyx/tree/develop.svg?style=svg)](https://circleci.com/gh/nlamirault/nyx/tree/develop)

A window manager writtent in Go and using the XCB wrappers.

## Installation

You can download the binaries :

* Architecture i386 [ [linux](https://bintray.com/artifact/download/nlamirault/oss/nyx_linux_386) / [darwin](https://bintray.com/artifact/download/nlamirault/oss/nyx_darwin_386) / [freebsd](https://bintray.com/artifact/download/nlamirault/oss/nyx_freebsd_386) / [netbsd](https://bintray.com/artifact/download/nlamirault/oss/nyx_netbsd_386) / [openbsd](https://bintray.com/artifact/download/nlamirault/oss/nyx_openbsd_386) / [windows](https://bintray.com/artifact/download/nlamirault/oss/nyx_windows_386.exe) ]
* Architecture amd64 [ [linux](https://bintray.com/artifact/download/nlamirault/oss/nyx_linux_amd64) / [darwin](https://bintray.com/artifact/download/nlamirault/oss/nyx_darwin_amd64) / [freebsd](https://bintray.com/artifact/download/nlamirault/oss/nyx_freebsd_amd64) / [netbsd](https://bintray.com/artifact/download/nlamirault/oss/nyx_netbsd_amd64) / [openbsd](https://bintray.com/artifact/download/nlamirault/oss/nyx_openbsd_amd64) / [windows](https://bintray.com/artifact/download/nlamirault/oss/nyx_windows_amd64.exe) ]
* Architecture arm [ [linux](https://bintray.com/artifact/download/nlamirault/oss/nyx_linux_arm) / [freebsd](https://bintray.com/artifact/download/nlamirault/oss/nyx_freebsd_arm) / [netbsd](https://bintray.com/artifact/download/nlamirault/oss/nyx_netbsd_arm) ]



## Testing

For testing, install [Xephyr][] on your system and run:

    $ Xephyr -screen 800x600 :1 &
    $ DISPLAY=:1 ./target/release/wtftw &
    $ DISPLAY=:1 thunar & (or whatever application you want to run)


## Development

* Initialize environment

        $ make init

* Build tool :

        $ make build

* Launch unit tests :

        $ make test

* To run and test it, you will need [Xephyr][]

        $ make run


## Contributing

See [CONTRIBUTING](CONTRIBUTING.md).


## License

See [LICENSE](LICENSE) for the complete license.


## Changelog

A [changelog](ChangeLog.md) is available


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>


[badge-license]: https://img.shields.io/badge/license-Apache2-green.svg?style=flat

[Xephyr]: https://www.freedesktop.org/wiki/Software/Xephyr/
