# create a single resource file testdata.lrs
# should run every time resource files change
$StartDir = Get-Location
$CurrentDir = Join-Path -Path $StartDir -ChildPath "tests"
$CurrentDir = Join-Path -Path $CurrentDir -ChildPath "resources"
Write-Host $StartDir
Write-Host $CurrentDir
Set-Location $CurrentDir
lazres ..\testdata.lrs .\leak_simple.trc
Set-Location $StartDir