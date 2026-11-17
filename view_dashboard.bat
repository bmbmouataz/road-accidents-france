@echo off
rem Double-click to open the dashboard: starts a local web server on docs/ and opens the browser.
rem Browsers block the dashboard's JavaScript when the page is opened directly from the disk.
cd /d "%~dp0docs"
echo.
echo   Dashboard: http://localhost:8000/site/dashboard.html
echo   Report:    http://localhost:8000/site/report.html
echo.
echo   Keep this window open while you browse. Close it to stop the server.
echo.
start "" cmd /c "timeout /t 2 >nul & start http://localhost:8000/site/dashboard.html"
python -m http.server 8000 --bind 127.0.0.1
