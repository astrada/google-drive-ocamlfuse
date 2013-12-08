In the basic procedure described in [[Usage]], you need a web browser for the authorization step.  On a headless server without a GUI interface or a web browser, you can get it to work by using a web browser on a separate computer, and pasting the results into the `~/.gdfuse/default/config` (or `~/.gdfuse/label/config` if you use labels) file of your headless server.

Caveat: I do not know whether this is the "optimal" way to do this.  I was in a pinch, and this approach worked.  At first, I had a difficult time getting it to work, but Alessandro responded immediately with help, and we got it to work.   These are the steps I followed, and of course, please edit this page if you have tips or a better approach.   

(I used the "Alternate Authorization Mode"  although the Standard Authorization mode using the GAE (Google App Engine) service may work too.)

1. Install normally as per the [[Installation]] page
1. In the computer with the web browser, create an OAuth2 application and credentials.
    These steps are similar to those described in the "Alternate Authorization Mode" section in [[Authorization]].  The following steps are what I did, based on my notes.  When I tried to retrace them, I got a message, which is either new or I did not notice it the first time, pointing me to Google's new [Cloud Console](https://cloud.google.com/console).   I did not use that, but I assume the steps will be similar.
    1. Sign in to your Google account and create a project: https://code.google.com/apis/console/
    1. Click "Services" in the left-hand pane (will take you to https://code.google.com/apis/console/?pli=1#:services). Turn on the service "Drive API".
    1. Click "API Access" in the left hand pane, then click on the button "Create an OAuth 2.0 Client ID …"
        1. Choose any product name, e.g "My OCAMLDrive".   Leave the rest of the fields blank
        1. Next page is "Client ID Setting".  Select Application Type:  "Installed Application", Installed Application Type:  "Other"
        1. Click "Create Client ID".  You will get a Client ID, a Client Secret, and a Redirect URIs.
           The latter should be  "urn:ietf:wg:oauth:2.0:oob".   I also had another Redirect URI,"http://localhost", which I ignored.

       Note also on this page a button for "Reset client secret…", which you shouldn't need to use, but I ended up using because I took too long to do these steps and the authorization code timed out.


1. Authorization: Back in your headless server, run `google-drive-ocamlfuse` for the first time. I used labels (in this document, I use the label "me") because I plan on using multiple accounts. However you can also run it without the `-label` parameter and it will use a default name for the label called "default". You will need the Client ID and secret you got from google above.

          $ google-drive-ocamlfuse -label me -id ##yourClientID##.apps.googleusercontent.com -secret ###yoursecret##### 

    Note:
    * the arguments are -id and -secret.  Elsewhere in the documentation outdated arguments are mentioned.

    This command will create the default application directory (`~/.gdfuse/me/`), containing the configuration file `config` (see the [[Configuration]] page). And it will try to start a web browser to obtain authorization.  Since there is no web browser present, you will get an error such as:

        /bin/sh: 1: xdg-open: not found
        /bin/sh: 1: firefox: not found
        /bin/sh: 1: google-chrome: not found
        Cannot retrieve auth tokens.
        Failure("Error opening URL:https://accounts.google.com/o/oauth2/auth?client_id=#####.apps.googleusercontent.com&redirect_uri=https%3A%2F%2Fgd-ocaml-auth.appspot.com%2Foauth2callback&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdrive&response_type=code&access_type=offline&approval_prompt=force&state=xxxxxxxxxxxxx")

    where I have substituted "xxxxxx" as needed for to obscure my personal info.

1. Copy the URL in the last line and use it in the browser of your other computer.

    You will get a dialog asking for authorization, and it will then respond with the verification code, which you will paste into the config file in the next step.
 
1. Now back on your headless computer, add your Client ID, secret, and verification code, to the following three lines in the `~/.gdfuse/label/config` file:
```
    verification_code=
    client_id=
    client_secret=
```
    You may want to edit other lines in the config file as per [[Configuration]].

1. That's it.  You should be ready to mount.

        mkdir /my_mnt_dir
        google-drive-ocamlfuse -label me /my_mnt_dir

1. Finally, to unmount, elsewhere in the instructions it says to use `fusermount -u mountpoint`.  That did not work for me for some reason, but `umount mountpoint` did.

Troubleshooting
---------------

When I first tried to mount my drive, I got an error message:


        Cannot retrieve auth tokens.
        Failure("OAuth2 error: invalid_grant (HTTP response code: 400)")


I then ran the same command above, with the addition of the -debug parameter, and still got an error:

		Starting application setup (label=me).
		Opening log file: /root/.gdfuse/me/gdfuse.log
		Cannot retrieve auth tokens.
		Failure("OAuth2 error: invalid_grant (HTTP response code: 400)")

This is where I got stuck but Alessandro helped me right away.  The problem was that my verification code had expired. I don't know what the timeout period is, but I just went back to the google page mentioned above, clicked on "Reset client secret…", which gave me a new secret id and secret, re-ran the auth page to get a new verification code, pasted  the new values into the `config` file, and then it worked like a charm. 