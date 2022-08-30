$CurrentPath = Get-Location
function Exit-Script {
  Set-Location $CurrentPath
  Exit
}

function Test-CommandExists
{
  param ($command)
  $oldPreference = $ErrorActionPreference
  $ErrorActionPreference = "stop"
  try {
    if (Get-Command $command) {
      RETURN $true
    }
  } catch {
    RETURN $false
  } finally {
    $ErrorActionPreference=$oldPreference
  }
}

function Get-BasePath {
  $CurrentDir = Get-Location
  if ($CurrentDir -notmatch '\\$') {
    $CurrentDir = Join-Path -Path $CurrentDir -ChildPath "\"
  }

  if ($CurrentDir -match 'generate\\$') {
    $CurrentDir = Join-Path -Path $CurrentDir "..\.."
    $CurrentDir = Resolve-Path -Path $CurrentDir
  }

  if ($CurrentDir -match 'docs\\$') {
    $CurrentDir = Join-Path -Path $CurrentDir ".."
    $CurrentDir = Resolve-Path -Path $CurrentDir
  }

  RETURN $CurrentDir 
}

$ItemAllUnits = '<li><a href="AllUnits.html">Units</a></li>'
$ItemAllIdentifiers = '<li><a href="AllIdentifiers.html">Identifiers</a></li>'
$ItemMenuRef = '</li></ul><hr><li>Reference'

function Set-NavigationItemOrder{
  param ($content, $navigationItem, $itemReference)
  $content = $content.Replace($navigationItem, '')
  $content = $content.Replace($itemReference, $navigationItem + $itemReference)
  return $content
}

function Set-NavigationMenuOrder {
  param ($file)
  
  $Content = Get-Content -Path $file

  $Content = Set-NavigationItemOrder $Content '<li><a href="install.html">Install</a></li>' $ItemAllUnits
  $Content = Set-NavigationItemOrder $Content '<li><a href="quickstart.html">Quick start</a></li>' $ItemAllUnits
  $Content = Set-NavigationItemOrder $Content '<li><a href="advanced.html">Advanced features</a></li>' $ItemAllUnits
  
  $Content = $Content.Replace($ItemAllUnits, '<hr><li>Reference<ul style="margin-left: 10px;">' + $ItemAllUnits)
  $Content = $Content.Replace($ItemAllIdentifiers, $ItemAllIdentifiers + '</li></ul>')

  $Content = $Content.Replace('<hr><li>Reference', '<hr><li>Commands<ul style="margin-left: 10px;"></li></ul><hr><li>Reference')

  $Content = Set-NavigationItemOrder $Content '<li><a href="command-add.html">Add</a></li>' $ItemMenuRef
  $Content = Set-NavigationItemOrder $Content '<li><a href="command-build.html">Build</a></li>' $ItemMenuRef
  $Content = Set-NavigationItemOrder $Content '<li><a href="command-clean.html">Clean</a></li>' $ItemMenuRef
  $Content = Set-NavigationItemOrder $Content '<li><a href="command-help.html">Help</a></li>' $ItemMenuRef
  $Content = Set-NavigationItemOrder $Content '<li><a href="command-install.html">Install</a></li>' $ItemMenuRef
  $Content = Set-NavigationItemOrder $Content '<li><a href="command-new.html">New</a></li>' $ItemMenuRef
  $Content = Set-NavigationItemOrder $Content '<li><a href="command-test.html">Test</a></li>' $ItemMenuRef
  $Content = Set-NavigationItemOrder $Content '<li><a href="command-version.html">Version</a></li>' $ItemMenuRef
  $Content = Set-NavigationItemOrder $Content '<li><a href="command-watch.html">Watch</a></li>' $ItemMenuRef

  Set-Content -Path $file -Value $Content
}

if (Test-CommandExists "pasdoc") {
  Write-Host "Pasdoc found"
} else {
  Write-Host "Pasdoc was not found, is required to build the documentation. Please install it or make sure that it is in current path."
  Exit-Script  
}

$BasePath = Get-BasePath
$DocArgs = 
  "--format=html " + 
  "--css=" + $BasePath + "docs\generate\custom.css " +
  "--use-tipue-search " +
  "--output " + $BasePath + "docs " +
  "--introduction=" + $BasePath + "docs\generate\introduction.txt " +
  "--additional="  + $BasePath + "docs\generate\install.txt " +
  "--additional="  + $BasePath + "docs\generate\quickstart.txt " +
  "--additional="  + $BasePath + "docs\generate\advanced.txt " +
  "--additional="  + $BasePath + "docs\generate\command-add.txt " +
  "--additional="  + $BasePath + "docs\generate\command-build.txt " +
  "--additional="  + $BasePath + "docs\generate\command-clean.txt " +
  "--additional="  + $BasePath + "docs\generate\command-help.txt " +
  "--additional="  + $BasePath + "docs\generate\command-install.txt " +
  "--additional="  + $BasePath + "docs\generate\command-new.txt " +
  "--additional="  + $BasePath + "docs\generate\command-test.txt " +
  "--additional="  + $BasePath + "docs\generate\command-version.txt " +
  "--additional="  + $BasePath + "docs\generate\command-watch.txt " +
  $BasePath + "src\commands\*.pas " + 
  $BasePath + "src\utils\*.pas " + 
  $BasePath + "pasc.lpr "

Start-Process -FilePath "pasdoc" -ArgumentList $DocArgs -Wait -NoNewWindow

$DocPath = Join-Path -Path $BasePath -ChildPath "docs\*.html"

Get-ChildItem -Path $DocPath -Recurse | ForEach-Object {
  Set-NavigationMenuOrder -file $_
}
