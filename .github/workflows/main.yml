name: freerdp6hours

on: workflow_dispatch

jobs:
  build:

    runs-on: windows-latest
    timeout-minutes: 9999

    steps:
    - name: Mendownload Ngrok.
      run: |
        Invoke-WebRequest https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-windows-amd64.zip -OutFile ngrok.zip
        Invoke-WebRequest https://raw.githubusercontent.com/Ibord1967/freerdp6hours/main/start.bat -OutFile start.bat
        Invoke-WebRequest https://raw.githubusercontent.com/Ibord1967/freerdp6hours/main/wallpaper.png -OutFile wallpaper.bmp
        Invoke-WebRequest https://raw.githubusercontent.com/Ibord1967/freerdp6hours/main/wallpaper.bat -OutFile wallpaper.bat
    - name: Mengextract Ngrok File.
      run: Expand-Archive ngrok.zip
    - name: Connect Ke Akun Ngrok.
      run: .\ngrok\ngrok.exe authtoken $Env:NGROK_AUTH_TOKEN
      env:
        NGROK_AUTH_TOKEN: ${{ secrets.NGROK_AUTH_TOKEN }}
    - name: Mengaktifkan Akses RDP.
      run: | 
        Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server'-name "fDenyTSConnections" -Value 0
        Enable-NetFirewallRule -DisplayGroup "Remote Desktop"
        Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -name "UserAuthentication" -Value 1
        copy wallpaper.bmp D:\a\wallpaper.bmp
        copy wallpaper.bat D:\a\wallpaper.bat
    - name: Membuat Tunnel.
      run: Start-Process Powershell -ArgumentList '-Noexit -Command ".\ngrok\ngrok.exe tcp 3389"'
    - name: Connect Ke RDP Kamu CPU 2 Core - 7GB Ram - 255 SSD.
      run: cmd /c start.bat
    - name: RDP Berhasil Dibuat! Anda Bisa Menutup Tab Sekarang. Jangan lupa subrek XP CHANNEL
      run: | 
        Invoke-WebRequest https://github.com/Ibord1967/freerdp6hours/raw/main/loop.ps1 -OutFile loop.ps1
        ./loop.ps1
