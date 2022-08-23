var tipuesearch = {"pages": [
     {"title": "introduction", "text": "     The pasc is a cross-platform command line tool that is intended to help with the development process of freepascal/lazarus projects, assisting during development, testing, build and documentation.    It allows you to create projects, add tests, add automation to documentation, in addition to providing a watch command to monitor the projects folder for changes and thereby trigger the build and test report process.    An interesting feature is the ease with which a new project can be configured to work with vscode providing configuration files for build, to run tests, and even for watch project folder and to trigger build and tests.    Commands              add    build    clean    help    install    new    test    version    watch          Requirements      This tool was tested with FPC 3.2.2 and Lazarus 2.2.2, but it may work with other versions. Should work under Linux, MacOS and Windows.    To work with the project using vscode you will going to need the following requirements:            a vscode extension that provides code completion, syntax check, etc for freepascal, OmniPascal is a good one.    a vscode extension to debug your project, the launch.json file generated by pasc are using the https://marketplace.visualstudio.com/items?itemName=webfreak.debug extension, but you can change it to one of your preference    vscode path should be in the path environment variable, it´s optional    lazbuild should be in the path, pasc uses it to build the projects and the tests projects    git tool for command line and in the path    boss https://github.com/HashLoad/boss a dependency manager tool, it´s optional    pasdoc should be in the path, pasc uses it to build documentation, it's optional          License      pasc is free and open-source software licensed under the MIT License.  Overview", "tags": "", "url": "introduction.html"},
     {"title": "install", "text": "     Pasc is a standalone executable application and it is simple to install.    Manual installation      Use the following link to download a zip file, extract its contents to a folder of preference, open a terminal and from this folder type the following:       ./pasc install        It will install itself on user's home folder creating a subfolder named .pasc and adding it to the path to make pasc visible when using the terminal app. It may be necessary to restart the terminal app for the changes take effect. Type pasc from the terminal app to check if it is working.    Using a script to download and install      Copy the script above and run it on a terminal:    Linux, MacOS:       curl -s http://server/path/script.sh | bash -s        Windows:       Invoke-WebRequest -Uri &quot;https://github.com/.../pasc.exe&quot; -OutFile &quot;./pasc.exe&quot;  Start-Process -FilePath &quot;./pasc&quot; -ArgumentList &quot;install&quot; -Wait        Build it from the sources      You may clone the repo and build it. Open a terminal and choose a folder to clone the repo and type the following commands:          git clone https://github.com/leandro-lprsoft/pasc.git  cd pasc  lazbuild pasc.lpi --build-mode=release  ./pasc install        Test pasc      To see if pasc is working type the following:          pasc help new        You should see an output like this:          pasc version 1.0.6    Usage: pasc new &lt;project file name&gt;    Creates a new free pascal program.  From the current path create a subfolder with the given project name,  initialize git, initialize the boss dependency manager, create build  and debug support files for use with vscode and create a project based  on a simple free pascal template.  Ex: pasc new &lt;project name>      Install", "tags": "", "url": "install.html"},
     {"title": "quickstart", "text": "     Assuming that pasc is already installed you can continue in this section.    Create a new project      Create a new project using pasc, open a terminal, navigate to your project path and run the following command:          pasc new helloWorld  code helloWorld        This command will create a subfolder named helloWorld and will create the project structure inside it. The second line calls vscode and opens the new folder that was created. Requires that vscode to be in the path.    Checking the project folder structure      You will notice that a project strucuture was created similar to this on below:          .  ├───.vscode  │   ├───launch.json  │   └───tasks.json  ├───src  ├───.gitignore  ├───boss-lock.json  ├───boss.json  ├───helloWorld.lpi  └───helloWorld.lpr        We have the project file and also a lazarus project file that makes easy to build the project. There are files to use vscode too with predefined tasks and a suggested launch entry to debug your app. Please check vscode requirements on install.    The folder was initialized as a git repository too and one suggested file as .gitignore based on pascal language.    The boss dependency manager was also initialized, so to install new dependencies using it is possible using the command &quot;boss install&quot;, for more information check https://github.com/HashLoad/boss. If you don't want to use boss just delete the filse boss-lock.json and boss.json from the root path of the project.    Type some code      If you ran the &quot;code helloWorld&quot; command, there should be a vscode instance with the project open. Activate vscode or just open the project folder in a new instance.    Open the helloWorld.lpr project file and enter the following code:          WriteLn('helloWorld');        If the prerequisites for using vscode are satisfied as described in install, you should be able to use the build task (ctrl + shift + b, or the appropriate combination depending on the platform).    The pasc tool doesn't compile the project it just creates tasks for it, and when it does it's just calling the Lazarus project build tool for that: lazbuild.    You can verify that the application has been compiled successfully by simply running the following command from a vscode terminal:          ./helloWorld        Using the vscode tasks      Some predefined tasks were created but only the build and launch for debug should work as long as the requirements for vscode operation are met.    Testing and documentation tasks require the existence of test projects and scripts for documentation automation that can be added by pasc via the add &ndash;tests &ndash;docs command. See the advanced on how to add tests and documentation to the project.  Quick start", "tags": "", "url": "quickstart.html"},
     {"title": "advanced", "text": "     Adding tests      Use the project from the previous page quickstart as a base to try out the features of this page.    To add a test project to the project, do the following: with the project folder open on vscode, open a terminal session and from the project root execute the following command:          pasc add --tests        This command will add a test project by creating a subfolder named tests and adding a basic test project template with some example cases. We can see that our project structure has been modified a bit to something like this:          .  ├───.vscode  │   ├───launch.json  │   └───tasks.json  ├───src  ├───tests  │   ├───TestHelloWorld.lpi  │   ├───TestHelloWorld.lpr  │   └───TestCaseHelloWorld.pas  ├───.gitignore  ├───boss-lock.json  ├───boss.json  ├───helloWorld.lpi  └───helloWorld.lpr        Open TestCaseHelloWorld.pas file, there are three tests in this unit, one test that passes, one that fails and one that causes a memory leak.    These tests are like this to demonstrate how the pasc test report looks like. The idea behind a different report is to provide a quick identification of tests that failed for some reason. And this is done through the use of colors and alignment and with the consolidation of test information and possible memory leaks in a single report.    Now our tests task can be run with a test project available, you can now run a vscode task &quot;pasc: run tests&quot;, it depends on &quot;lazbuild: build tests&quot;. You should see an output similar to this:    Adding documentation      Watching for changes    Advanced features", "tags": "", "url": "advanced.html"},
     {"title": "Command.Add", "text": "   ", "tags": "", "url": "Command.Add.html"},
     {"title": "Command.Add.AddCommand", "text": "   ABuilder Command builder of the main application that will be used to output user instructions about the execution state of this command. ABuilder Command builder of the main application that will be used to output user instructions about the execution state of this command. ", "tags": "", "url": "Command.Add.html#AddCommand"},
     {"title": "Command.Add.Registry", "text": "   ABuilder Command builder of the main application that will be used to registry the command. ABuilder Command builder of the main application that will be used to registry the command. ", "tags": "", "url": "Command.Add.html#Registry"},
     {"title": "Command.Build", "text": "   ", "tags": "", "url": "Command.Build.html"},
     {"title": "Command.Build.BuildCommand", "text": "   ABuilder Command builder that will provide the output callback to print info about the command execution ABuilder Command builder that will provide the output callback to print info about the command execution ", "tags": "", "url": "Command.Build.html#BuildCommand"},
     {"title": "Command.Build.Registry", "text": "   ABuilder Command builder of the main application that will be used to registry the command. ABuilder Command builder of the main application that will be used to registry the command. ", "tags": "", "url": "Command.Build.html#Registry"},
     {"title": "Command.Build.FindProject", "text": "       AProjectDir Folder to find the lazarus project AProjectFile Project file specified. AProjectDir Folder to find the lazarus project AProjectFile Project file specified. ", "tags": "", "url": "Command.Build.html#FindProject"},
     {"title": "Command.Clean", "text": "   ", "tags": "", "url": "Command.Clean.html"},
     {"title": "Command.Clean.CleanCommand", "text": "   ABuilder Command builder that will provide the output callback to print info about the command execution ABuilder Command builder that will provide the output callback to print info about the command execution ", "tags": "", "url": "Command.Clean.html#CleanCommand"},
     {"title": "Command.Clean.Registry", "text": "   ABuilder Command builder of the main application that will be used to registry the command. ABuilder Command builder of the main application that will be used to registry the command. ", "tags": "", "url": "Command.Clean.html#Registry"},
     {"title": "Command.Clean.AnsweredAll", "text": "   ", "tags": "", "url": "Command.Clean.html#AnsweredAll"},
     {"title": "Command.Install", "text": "   ", "tags": "", "url": "Command.Install.html"},
     {"title": "Command.Install.InstallCommand", "text": "   ABuilder Instance o the builder that will be use to output instructions to user about the installation status ABuilder Instance o the builder that will be use to output instructions to user about the installation status ", "tags": "", "url": "Command.Install.html#InstallCommand"},
     {"title": "Command.Install.Registry", "text": "   ABuilder Command builder of the main application that will be used to registry the command. ABuilder Command builder of the main application that will be used to registry the command. ", "tags": "", "url": "Command.Install.html#Registry"},
     {"title": "Command.New", "text": "   ", "tags": "", "url": "Command.New.html"},
     {"title": "Command.New.NewCommand", "text": "   ABuilder Command builder of the main application that will be used to output user instructions about the execution state of this command. ABuilder Command builder of the main application that will be used to output user instructions about the execution state of this command. ", "tags": "", "url": "Command.New.html#NewCommand"},
     {"title": "Command.New.Registry", "text": "   ABuilder Command builder of the main application that will be used to registry the command. ABuilder Command builder of the main application that will be used to registry the command. ", "tags": "", "url": "Command.New.html#Registry"},
     {"title": "Command.Test", "text": "   ", "tags": "", "url": "Command.Test.html"},
     {"title": "Command.Test.TestCommand", "text": "   ABuilder Command builder of the main application that will be used to output user instructions about the execution state of this command. ABuilder Command builder of the main application that will be used to output user instructions about the execution state of this command. ", "tags": "", "url": "Command.Test.html#TestCommand"},
     {"title": "Command.Test.Registry", "text": "   ABuilder Command builder of the main application that will be used to registry the command. ABuilder Command builder of the main application that will be used to registry the command. ", "tags": "", "url": "Command.Test.html#Registry"},
     {"title": "Command.Test.GetTestExecutable", "text": "   ABuilder Command builder of the main application that will be used to output info about the existence of the test project and whether it is valid. ABuilder Command builder of the main application that will be used to output info about the existence of the test project and whether it is valid. ", "tags": "", "url": "Command.Test.html#GetTestExecutable"},
     {"title": "Command.Watch", "text": "( This unit contains procedures for setting up and executing a watch command in the current directory. If there are changes to files in that directory, specified commands are executed. &lt;/sumamry&gt;   ", "tags": "", "url": "Command.Watch.html"},
     {"title": "Command.Watch.WatchCommand", "text": "   ABuilder Command builder of the main application that will be used to output user instructions about the execution state of this command. ABuilder Command builder of the main application that will be used to output user instructions about the execution state of this command. ", "tags": "", "url": "Command.Watch.html#WatchCommand"},
     {"title": "Command.Watch.Registry", "text": "   ABuilder Command builder of the main application that will be used to registry the command. ABuilder Command builder of the main application that will be used to registry the command. ", "tags": "", "url": "Command.Watch.html#Registry"},
     {"title": "Command.Watch.RunUserCommandAsRequested", "text": "   AFile Filename that triggered the action change. AFile Filename that triggered the action change. ", "tags": "", "url": "Command.Watch.html#RunUserCommandAsRequested"},
     {"title": "Command.Watch.RunWatcherCallback", "text": "   ", "tags": "", "url": "Command.Watch.html#RunWatcherCallback"},
     {"title": "Command.Watch.CommandWatchTimeout", "text": "   ", "tags": "", "url": "Command.Watch.html#CommandWatchTimeout"},
     {"title": "pasc", "text": "   ", "tags": "", "url": "pasc.html"},
     {"title": "Utils.Interfaces", "text": "   ", "tags": "", "url": "Utils.Interfaces.html"},
     {"title": "Utils.Interfaces.IPathWatcher", "text": "ignore files with the specified extension    ", "tags": "", "url": "Utils.Interfaces.IPathWatcher.html"},
     {"title": "Utils.Interfaces.IPathWatcher.Path", "text": "   APath Path that will be monitored APath Path that will be monitored ", "tags": "", "url": "Utils.Interfaces.IPathWatcher.html#Path"},
     {"title": "Utils.Interfaces.IPathWatcher.Ignore", "text": "       AIgnoreKind Kind of item to be ignored by the watcher AItems An array containing strings to be ignored according to AIgnoreKind AIgnoreKind Kind of item to be ignored by the watcher AItems An array containing strings to be ignored according to AIgnoreKind ", "tags": "", "url": "Utils.Interfaces.IPathWatcher.html#Ignore"},
     {"title": "Utils.Interfaces.IPathWatcher.Timeout", "text": "   ATime Number of milliseconds to set the maximum timeout. ATime Number of milliseconds to set the maximum timeout. ", "tags": "", "url": "Utils.Interfaces.IPathWatcher.html#Timeout"},
     {"title": "Utils.Interfaces.IPathWatcher.Run", "text": "     )  AProc A procedure that will be called after a change AProc A procedure that will be called after a change ", "tags": "", "url": "Utils.Interfaces.IPathWatcher.html#Run"},
     {"title": "Utils.Interfaces.IPathWatcher.Start", "text": "    ", "tags": "", "url": "Utils.Interfaces.IPathWatcher.html#Start"},
     {"title": "Utils.Interfaces.TWatcherRunCallback", "text": "    ", "tags": "", "url": "Utils.Interfaces.html#TWatcherRunCallback"},
     {"title": "Utils.Interfaces.TIgnoreKind", "text": "    ikStartsText    ikFolder  itens of any kind that starts with the text specified  ikFile  ignore folders with the specified name  ikExtension  ignore files with the specified name ", "tags": "", "url": "Utils.Interfaces.html#TIgnoreKind"},
     {"title": "Utils.IO", "text": "   ", "tags": "", "url": "Utils.IO.html"},
     {"title": "Utils.IO.GetFileContent", "text": "    File name that will have its content returned )    ", "tags": "", "url": "Utils.IO.html#GetFileContent"},
     {"title": "Utils.IO.GetJsonFileContentWithOutComments", "text": "    File name that will have its content returned )    ", "tags": "", "url": "Utils.IO.html#GetJsonFileContentWithOutComments"},
     {"title": "Utils.IO.SaveFileContent", "text": "       AFileName Target file name, the file name will be replaced AContent Content string that will be saved on the file AFileName Target file name, the file name will be replaced AContent Content string that will be saved on the file ", "tags": "", "url": "Utils.IO.html#SaveFileContent"},
     {"title": "Utils.IO.FindInCodeFile", "text": "       ACodeFile Should have have the file contents already loaded. AText The text to be found ACodeFile Should have have the file contents already loaded. AText The text to be found ", "tags": "", "url": "Utils.IO.html#FindInCodeFile"},
     {"title": "Utils.IO.FindFile", "text": "       ACurrentDir Folder from which to search including subfolders AFileName Name of the file to be searched ACurrentDir Folder from which to search including subfolders AFileName Name of the file to be searched ", "tags": "", "url": "Utils.IO.html#FindFile"},
     {"title": "Utils.IO.FindProjectFile", "text": "   ABuilder Command builder of the main application that will be used to output user instructions or to iteract with the user. ABuilder Command builder of the main application that will be used to output user instructions or to iteract with the user. ", "tags": "", "url": "Utils.IO.html#FindProjectFile"},
     {"title": "Utils.IO.FindSourceFile", "text": "   AProjectDir Project path from which to search AProjectDir Project path from which to search ", "tags": "", "url": "Utils.IO.html#FindSourceFile"},
     {"title": "Utils.Leak", "text": "   ", "tags": "", "url": "Utils.Leak.html"},
     {"title": "Utils.Leak.TLeakItem", "text": "   ", "tags": "", "url": "Utils.Leak.TLeakItem.html"},
     {"title": "Utils.Leak.TLeakItem.New", "text": "    ", "tags": "", "url": "Utils.Leak.TLeakItem.html#New"},
     {"title": "Utils.Leak.TLeakItem.Status", "text": "   ", "tags": "", "url": "Utils.Leak.TLeakItem.html#Status"},
     {"title": "Utils.Leak.TLeakItem.Size", "text": "   ", "tags": "", "url": "Utils.Leak.TLeakItem.html#Size"},
     {"title": "Utils.Leak.TLeakItem.Source", "text": "   ", "tags": "", "url": "Utils.Leak.TLeakItem.html#Source"},
     {"title": "Utils.Leak.TLeakReport", "text": "&lt;sumary&gt; This class aims to interpret the memory leak trace file and produce a simpler report summarizing enough information to locate the problem at its source. )   ", "tags": "", "url": "Utils.Leak.TLeakReport.html"},
     {"title": "Utils.Leak.TLeakReport.Create", "text": "    ", "tags": "", "url": "Utils.Leak.TLeakReport.html#Create"},
     {"title": "Utils.Leak.TLeakReport.Destroy", "text": "    ", "tags": "", "url": "Utils.Leak.TLeakReport.html#Destroy"},
     {"title": "Utils.Leak.TLeakReport.New", "text": "    A valid instance of ICommandBuilder. Basically it will be used to generate the output to the console considering the theme settings.)    The path to the source code, needed to find the files corresponding to the source code reported in the memory leak trace file. )    ", "tags": "", "url": "Utils.Leak.TLeakReport.html#New"},
     {"title": "Utils.Leak.TLeakReport.AddItem", "text": "    ", "tags": "", "url": "Utils.Leak.TLeakReport.html#AddItem"},
     {"title": "Utils.Leak.TLeakReport.CreateLeakItem", "text": "    ", "tags": "", "url": "Utils.Leak.TLeakReport.html#CreateLeakItem"},
     {"title": "Utils.Leak.TLeakReport.GetNextStringOf", "text": "    ", "tags": "", "url": "Utils.Leak.TLeakReport.html#GetNextStringOf"},
     {"title": "Utils.Leak.TLeakReport.AddRelativePath", "text": "    ", "tags": "", "url": "Utils.Leak.TLeakReport.html#AddRelativePath"},
     {"title": "Utils.Leak.TLeakReport.Output", "text": "    ", "tags": "", "url": "Utils.Leak.TLeakReport.html#Output"},
     {"title": "Utils.Leak.TLeakReport.ParseHeapTrace", "text": "   AContent Accepts the contents of the memory leak trace file. If an empty string is passed, the method will try to locate the heap.trc file in the test project's executable directory. AContent Accepts the contents of the memory leak trace file. If an empty string is passed, the method will try to locate the heap.trc file in the test project's executable directory. ", "tags": "", "url": "Utils.Leak.TLeakReport.html#ParseHeapTrace"},
     {"title": "Utils.Leak.TLeakReport.Executable", "text": "   ", "tags": "", "url": "Utils.Leak.TLeakReport.html#Executable"},
     {"title": "Utils.Leak.TLeakReport.ProjectSource", "text": "   ", "tags": "", "url": "Utils.Leak.TLeakReport.html#ProjectSource"},
     {"title": "Utils.Leak.TLeakReport.LeakData", "text": "   ", "tags": "", "url": "Utils.Leak.TLeakReport.html#LeakData"},
     {"title": "Utils.Output", "text": "   ", "tags": "", "url": "Utils.Output.html"},
     {"title": "Utils.Output.OutputInfo", "text": "       ATitle Title highlighted using the theme's Title color property AText information description without accent color, uses the theme's Other color property ATitle Title highlighted using the theme's Title color property AText information description without accent color, uses the theme's Other color property ", "tags": "", "url": "Utils.Output.html#OutputInfo"},
     {"title": "Utils.Output.OutputError", "text": "       ATitle Title highlighted using the theme's Title color property AText information description with accent color, uses the theme's Error color property ATitle Title highlighted using the theme's Title color property AText information description with accent color, uses the theme's Error color property ", "tags": "", "url": "Utils.Output.html#OutputError"},
     {"title": "Utils.Resources", "text": "   ", "tags": "", "url": "Utils.Resources.html"},
     {"title": "Utils.Resources.GetResource", "text": "    Resource name to get the string from )    ", "tags": "", "url": "Utils.Resources.html#GetResource"},
     {"title": "Utils.Shell", "text": "   ", "tags": "", "url": "Utils.Shell.html"},
     {"title": "Utils.Shell.ShellCommand", "text": "       AProgram Program name or command to be executed. AParams Array of argumentos to be passed to the program or command being called AProgram Program name or command to be executed. AParams Array of argumentos to be passed to the program or command being called ", "tags": "", "url": "Utils.Shell.html#ShellCommand"},
     {"title": "Utils.Shell.TShellCommandFunc", "text": "    ", "tags": "", "url": "Utils.Shell.html#TShellCommandFunc"},
     {"title": "Utils.Shell.ShellExecute", "text": "   ", "tags": "", "url": "Utils.Shell.html#ShellExecute"},
     {"title": "Utils.Tests", "text": "   ", "tags": "", "url": "Utils.Tests.html"},
     {"title": "Utils.Tests.TTestCaseItem", "text": "   ", "tags": "", "url": "Utils.Tests.TTestCaseItem.html"},
     {"title": "Utils.Tests.TTestCaseItem.New", "text": "    ", "tags": "", "url": "Utils.Tests.TTestCaseItem.html#New"},
     {"title": "Utils.Tests.TTestCaseItem.Status", "text": "   ", "tags": "", "url": "Utils.Tests.TTestCaseItem.html#Status"},
     {"title": "Utils.Tests.TTestCaseItem.Time", "text": "   ", "tags": "", "url": "Utils.Tests.TTestCaseItem.html#Time"},
     {"title": "Utils.Tests.TTestCaseItem.TestSuite", "text": "   ", "tags": "", "url": "Utils.Tests.TTestCaseItem.html#TestSuite"},
     {"title": "Utils.Tests.TTestCaseItem.TestCase", "text": "   ", "tags": "", "url": "Utils.Tests.TTestCaseItem.html#TestCase"},
     {"title": "Utils.Tests.TTestCaseItem.Error", "text": "   ", "tags": "", "url": "Utils.Tests.TTestCaseItem.html#Error"},
     {"title": "Utils.Tests.TTestReport", "text": "&lt;sumary&gt; This class aims to interpret the fpcunit test xml file and produce a simpler report summarizing all tests cases and also providing their location on source code. )   ", "tags": "", "url": "Utils.Tests.TTestReport.html"},
     {"title": "Utils.Tests.TTestReport.Create", "text": "    ", "tags": "", "url": "Utils.Tests.TTestReport.html#Create"},
     {"title": "Utils.Tests.TTestReport.Destroy", "text": "    ", "tags": "", "url": "Utils.Tests.TTestReport.html#Destroy"},
     {"title": "Utils.Tests.TTestReport.New", "text": "    A valid instance of ICommandBuilder. Basically it will be used to generate the output to the console considering the theme settings.)    The path to the source code, needed to find the files corresponding to the source code reported in the fpcunit xml test file. )    ", "tags": "", "url": "Utils.Tests.TTestReport.html#New"},
     {"title": "Utils.Tests.TTestReport.ParseXmlTestsFile", "text": "       ATestApp Test application name that will be printed along with the test report output AFileName Unit test xml file generated by the test application ATestApp Test application name that will be printed along with the test report output AFileName Unit test xml file generated by the test application ", "tags": "", "url": "Utils.Tests.TTestReport.html#ParseXmlTestsFile"},
     {"title": "Utils.Tests.TTestReport.AddItem", "text": "   AItem The item to be added to TestCaseData AItem The item to be added to TestCaseData ", "tags": "", "url": "Utils.Tests.TTestReport.html#AddItem"},
     {"title": "Utils.Tests.TTestReport.Output", "text": "    ", "tags": "", "url": "Utils.Tests.TTestReport.html#Output"},
     {"title": "Utils.Tests.TTestReport.Executable", "text": "   ", "tags": "", "url": "Utils.Tests.TTestReport.html#Executable"},
     {"title": "Utils.Tests.TTestReport.ProjectSource", "text": "   ", "tags": "", "url": "Utils.Tests.TTestReport.html#ProjectSource"},
     {"title": "Utils.Tests.TTestReport.TestCaseCount", "text": "   ", "tags": "", "url": "Utils.Tests.TTestReport.html#TestCaseCount"},
     {"title": "Utils.Tests.TTestReport.TestSuiteCount", "text": "   ", "tags": "", "url": "Utils.Tests.TTestReport.html#TestSuiteCount"},
     {"title": "Utils.Tests.TTestReport.TotalTime", "text": "   ", "tags": "", "url": "Utils.Tests.TTestReport.html#TotalTime"},
     {"title": "Utils.Tests.TTestReport.TestsPassed", "text": "   ", "tags": "", "url": "Utils.Tests.TTestReport.html#TestsPassed"},
     {"title": "Utils.Tests.TTestReport.TestsFailed", "text": "   ", "tags": "", "url": "Utils.Tests.TTestReport.html#TestsFailed"},
     {"title": "Utils.Tests.TTestReport.TestCaseData", "text": "   ", "tags": "", "url": "Utils.Tests.TTestReport.html#TestCaseData"},
     {"title": "Utils.Watcher", "text": "   ", "tags": "", "url": "Utils.Watcher.html"},
     {"title": "Utils.Watcher.TPathWatcher", "text": "   ", "tags": "", "url": "Utils.Watcher.TPathWatcher.html"},
     {"title": "Utils.Watcher.TPathWatcher.Create", "text": "    ", "tags": "", "url": "Utils.Watcher.TPathWatcher.html#Create"},
     {"title": "Utils.Watcher.TPathWatcher.Destroy", "text": "    ", "tags": "", "url": "Utils.Watcher.TPathWatcher.html#Destroy"},
     {"title": "Utils.Watcher.TPathWatcher.New", "text": "    ", "tags": "", "url": "Utils.Watcher.TPathWatcher.html#New"},
     {"title": "Utils.Watcher.TPathWatcher.Path", "text": "   APath Path that will be monitored APath Path that will be monitored ", "tags": "", "url": "Utils.Watcher.TPathWatcher.html#Path"},
     {"title": "Utils.Watcher.TPathWatcher.Ignore", "text": "       AIgnoreKind Kind of item to be ignored by the watcher AItems An array containing strings to be ignored according to AIgnoreKind AIgnoreKind Kind of item to be ignored by the watcher AItems An array containing strings to be ignored according to AIgnoreKind ", "tags": "", "url": "Utils.Watcher.TPathWatcher.html#Ignore"},
     {"title": "Utils.Watcher.TPathWatcher.Timeout", "text": "   ATime Number of milliseconds to set the maximum timeout. ATime Number of milliseconds to set the maximum timeout. ", "tags": "", "url": "Utils.Watcher.TPathWatcher.html#Timeout"},
     {"title": "Utils.Watcher.TPathWatcher.Run", "text": "     )  AProc A procedure that will be called after a change AProc A procedure that will be called after a change ", "tags": "", "url": "Utils.Watcher.TPathWatcher.html#Run"},
     {"title": "Utils.Watcher.TPathWatcher.Start", "text": "    ", "tags": "", "url": "Utils.Watcher.TPathWatcher.html#Start"},
     {"title": "Utils.Watcher.TDictFile", "text": "   ", "tags": "", "url": "Utils.Watcher.html#TDictFile"}
]};
