#!/usr/bin/env pwsh
##############################################################################################################

Function Show-Usage {
    "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
" | Out-Host
}

Function Request-File {
    While ($Input.MoveNext()) {
        $VAR = @{
            Uri = $Input.Current
            OutFile = (Split-Path -Path $Input.Current -Leaf).Split('?')[0]
        }
        Invoke-WebRequest @VAR
        Return $VAR.OutFile
    }
}

Function Install-Program {
    While ($Input.MoveNext()) {
        Switch ((Split-Path -Path $Input.Current -Leaf).Split('.')[-1]) {
            'msi' {
                & msiexec /passive /package $Input.Current | Out-Host
            }
            Default {
                & ".\$($Input.Current)" /SP- /VERYSILENT /SUPPRESSMSGBOXES /NORESTART | Out-Host
            }
        }
        Remove-Item $Input.Current
    }
}

Function Build-Project {
    @(
        @{
            Cmd = 'lazbuild'
            Url = 'https://fossies.org/windows/misc/lazarus-3.6-fpc-3.2.2-win64.exe'
            Path = "C:\Lazarus"
        }
    ) | ForEach-Object {
        If (! (Test-Path -Path $_.Path)) {
            $_.Url | Request-File | Install-Program
            $env:PATH+=";$($_.Path)"
            ".... install $((Get-Command $_.Cmd).Source)" | Out-Host
        }
    }
    $VAR = @{
        Src = 'src'
        Use = 'use'
        Pkg = 'use\components.txt'
    }
    If (Test-Path -Path $VAR.Use) {
        If ((Test-Path -Path '.gitmodules') &&
            (& git submodule update --init --recursive --force --remote)) {
                ".... [$($LastExitCode)] git submodule update" | Out-Host
            }
        If (Test-Path -Path $VAR.Pkg) {
            Get-Content -Path $VAR.Pkg | ForEach-Object {
                If ((! (& lazbuild --verbose-pkgsearch $_ )) &&
                    (! (& lazbuild --add-package $_)) &&
                    (! (Test-Path -Path "$($VAR.Use)\$($_)"))) {
                        $OutFile = "https://packages.lazarus-ide.org/$($_).zip" | Request-File
                        Expand-Archive -Path $OutFile -DestinationPath "$($VAR.Use)\$($_)" -Force
                        Remove-Item $OutFile
                        ".... download package $($_)" | Out-Host
                    }
            }
        }
        Get-ChildItem -Filter '*.lpk' -Recurse -File –Path $VAR.Use |
            ForEach-Object { $Result = @() } {
                If (& lazbuild --add-package-link $_) {
                    $Result += ".... [$($LastExitCode)] add dependence $($_)"
                }
            } { $Result | Out-Host }
    }
    Get-ChildItem -Filter '*.lpi' -Recurse -File –Path $VAR.Src |
        Where-Object { $_ -notlike 'backup' } |
        ForEach-Object {
            $Result = @()
            $exitCode = 0
        } {
            $Tmp = (New-TemporaryFile).Name
            & lazbuild --build-all --recursive --no-write-project --build-mode='release' $_ |
                Out-File -FilePath $Tmp -Force
            If ($LastExitCode -eq 0) {
                $Result += Get-Content -Path $Tmp | Select-String -Pattern 'Linking'
            } Else {
                $exitCode+=1
                $Result += Get-Content -Path $Tmp | Select-String -Pattern 'Error:', 'Fatal:'
            }
            Remove-Item $Tmp
        } {
            $Result | Out-Host
            Exit $exitCode
        }
}

Function Switch-Action {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict #-Trace 1
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        Switch ($args[0]) {
            'build' {
                Build-Project
            }
            Default {
                Show-Usage
            }
        }
    } Else {
        Show-Usage
    }
}

##############################################################################################################
Switch-Action @args | Out-Null
