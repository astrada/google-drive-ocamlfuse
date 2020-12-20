#!/bin/zsh

# Uses nvm or you may configure Node path manually
# Mounts to /mnt/gdrive_<username>
# Optional crontab entry: * * * * * /<path>/get_auth_code.sh
# Checks if directory is not empty or exists, if not it reauthenticates for code and remounts

export APPID='xxxxxx-pf1534ph6iitt1dbouvhmk3qil2tee6i.apps.googleusercontent.com'
export KEY='xxxxVExuumqPxD1kksA7WN'
export U='your_google_username'
export P='your_google_password'
export PATH=$PATH:/usr/bin
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"

[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

alias gdrivelogin="node login_puppet.js $U $P $APPID"
alias gdrivemounter="mkdir /mnt/gdrive_$U;  fusermount -u  /mnt/gdrive_$U; google-drive-ocamlfuse -headless -label me -id $APPID  -secret $KEY; google-drive-ocamlfuse -label me /mnt/gdrive_$U;"
alias killgdrive="ps ax | grep 'google-drive-ocamlfuse' | awk -F ' ' '{print $1}' | xargs sudo kill -9"

if [ "$(ls -A /mnt/gdrive_$U)" ]; then
        echo "GDrive Up";
else
        sed -i ~/.gdfuse/me/config -e "s/verification_code=.*/verification_code="$(echo `gdrivelogin | tr -d '\n'` | sed -e 's/[]\/$*.^[]/\\&/g')"/g";
        gdrivemounter;
fi
