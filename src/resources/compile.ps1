# create a single resource file pasc.lrs
# should run every time resource files change
$StartDir = Get-Location
$CurrentDir = Join-Path -Path $StartDir -ChildPath "src"
$CurrentDir = Join-Path -Path $CurrentDir -ChildPath "resources"
Write-Host $StartDir
Write-Host $CurrentDir
Set-Location $CurrentDir
lazres ..\utils\pasc.lrs .\gitignore.txt .\launchjson.txt .\tasksjson.txt .\projectlpi.txt .\projectlpr.txt .\update-path-ps1.txt
Set-Location $StartDir