## pasc
pasc is a command line tool to help the developer manage free pascal projects with boiler plate tasks like git repository config, launch tasks, test projects creation and others.

## Install

* **Manual installation**: Download the last release and make sure that pasc binary is in the path.

* **use pasc command to install it on path:**
```console
./pasc --install
```

* **Install from sources:**: Clone the repository or download the sources. Make sure lazbuild.exe is in the path and run the following command:

```console
cd pasc
lazbuild ./src/pasc.lpi
./src/pasc --install
```

## Quickstart

Use pasc to create a simple console program:

```console
pasc new --console myapp.lpr
cd myapp
code .
```

## Features

* Quick crete new free pascal projects, initilizing git repository, with standardized folder structure, create tasks for build amd debug (based on GDB Debugger - Beyond, Native debug).

* Watch sources to continuous build after each save and to run tests.

* Allow add tests folder standard with test console app base on fpcunit.

* Clean command to erase lib and backups folders

## License

`pasc` is free and open-source software licensed under the [MIT License](https://github.com/leandro-lprsoft/pasc/blob/master/LICENSE). 