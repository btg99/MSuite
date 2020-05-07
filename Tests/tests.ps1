dotnet publish ../Miml -r win-x64 -p:PublishSingleFile=true -o .
dotnet publish ../Mint -r win-x64 -p:PublishSingleFile=true -o .

Get-ChildItem -Include *.miml |
    ForEach-Object -Process {
        $basename = $_.Basename
        $expected = Get-Item "$basename.expected"
        if (expected.Exists) {
            ./Miml.exe $_.Name
            ./Mint.exe -input $basename -output "$basename.actual"
            $expected = Get-Content $expected
        }
    } 