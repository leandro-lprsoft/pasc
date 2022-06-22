# create a single resource file pasc.lrs
# should run every time resource files change
StartDir=$PWD
CurrentDir=$StartDir"/src/resources"
echo $StartDir
echo $CurrentDir
cd $CurrentDir
lazres ../utils/pasc.lrs ./gitignore.txt ./launchjson.txt ./tasksjson.txt ./projectlpi.txt ./projectlpr.txt ./update-path-ps1.txt ./update-path-sh.txt
cd $StartDir