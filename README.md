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

* Quick crete new free pascal projects, initilizing git repository, with standardized folder structure, create tasks for build amd debug (based on Native debug vscode extension).

* Watch sources to continuous build after each save and to run tests.

* Allow add tests folder standard with test console app base on fpcunit.

* Clean command to erase lib and backups folders

* Test command to output a clean view of tests results

* Memory leak reported after the test report. To work it is necessary to turn on the option "Use HeapTrace unit" located on project options, compiler options, debugging section of lazarus IDE.

Add this unit to the uses clause:
```pascal
  uses SysUtils;
```

Add this code to main block of your test program (remove the tests sub folder if the test project is not inside it):
```pascal
  {$IF DECLARED(UseHeapTrace)}
  if FileExists('tests/heap.trc') then
    DeleteFile('tests/heap.trc');
  SetHeapTraceOutput('tests/heap.trc');
  {$ENDIF}
```

## License

`pasc` is free and open-source software licensed under the [MIT License](https://github.com/leandro-lprsoft/pasc/blob/master/LICENSE). 