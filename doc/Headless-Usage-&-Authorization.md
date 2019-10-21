In the basic procedure described in [Usage](Usage.md), you need a web browser for the authorization step.  On a headless server without a GUI interface or a web browser, you can get it to work by using a web browser on a separate computer, and pasting the results into the `~/.gdfuse/default/config` (or `~/.gdfuse/label/config` if you use labels) file of your headless server.

Caveat: I do not know whether this is the "optimal" way to do this.  I was in a pinch, and this approach worked.  At first, I had a difficult time getting it to work, but Alessandro responded immediately with help, and we got it to work.   These are the steps I followed, and of course, please edit this page if you have tips or a better approach.   

(I used the "Alternate Authorization Mode"  although the Standard Authorization mode using the GAE (Google App Engine) service may work too.)

1. Install normally as per the [Installation](Installation.md) page
1. On the computer with the web browser, create an OAuth2 application and credentials. These steps are similar to those described in the "Alternate Authorization Mode" section in [Authorization](Authorization.md). The following steps are what I did, based on my notes:
    1. Sign in to your Google account and create a project: https://console.cloud.google.com/
    1. Enable the Google Drive API
        1. In the left-hand pane (Navigation menu), open "APIs & Services" -> "Library", this will take you to https://console.cloud.google.com/apis/library
        1. Click on "Google Drive API", this will open https://console.cloud.google.com/apis/library/drive.googleapis.com . Click "ENABLE API".
    1. Get your client ID and client secret
        1. Open the Navigation menu (click the hamburger icon at the top left), open "APIs & Services" -> "Credentials", this will open https://console.cloud.google.com/apis/credentials
        1. Then click on the button "Create Credentials", and choose choose "OAuth client ID", this will take you to https://console.cloud.google.com/apis/credentials/oauthclient .
            * For Application type, choose "Other"
            * For Name, input something personal, for example "My OCAMLDrive".
        1. Click "Create". You will get a Client ID, a Client Secret.

1. Authorization: Back in your headless server, run `google-drive-ocamlfuse` for the first time. I used labels (in this document, I use the label "me") because I plan on using multiple accounts. However you can also run it without the `-label` parameter and it will use a default name for the label called "default". You will need the Client ID and secret you got from google above.

    From version 0.5.3, you should use the `-headless` option:

          $ google-drive-ocamlfuse -headless -label me -id ##yourClientID##.apps.googleusercontent.com -secret ###yoursecret##### 

    You will get an output like this:

          Please, open the following URL in a web browser: https://accounts.google.com/o/oauth2/auth?client_id=##yourClientID##.apps.googleusercontent.com&redirect_uri=urn%3Aietf%3Awg%3Aoauth%3A2.0%3Aoob&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdrive&response_type=code&access_type=offline&approval_prompt=force

    Then, you should open the URL above in a graphical web browser, get the verification code from Google
    and put it here:

          Please enter the verification code: 

1. That's it. You should be ready to mount.

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
