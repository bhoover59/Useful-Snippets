% Script to send email
% Need to authorize gmail: see instructions at bottom of script
setpref('Internet','SMTP_Server','smtp.gmail.com'); % have only tested with gmail, should work with mail.yahoo
setpref('Internet','E_mail','yourgemail@gmail.com'); % your gmail
setpref('Internet','SMTP_Username','yourgmail@gmail.com'); % your username is your whole gmail
setpref('Internet','SMTP_Password','your16digitcode'); % 16 digit code from changing settings
props = java.lang.System.getProperties;
props.setProperty('mail.smtp.auth','true');
props.setProperty('mail.smtp.socketFactory.class', 'javax.net.ssl.SSLSocketFactory');
props.setProperty('mail.smtp.socketFactory.port','465');

sendmail('recipientEmail','Text of message you want to send') ;

%{
Sign in with App Passwords
Tip: App Passwords aren’t recommended and are unnecessary in most cases. To help keep your account secure, use "Sign in with Google" to connect apps to your Google Account. 
An App Password is a 16-digit passcode that gives a less secure app or device permission to access your Google Account. App Passwords can only be used with accounts that have 2-Step Verification turned on.

Go to your Google Account.
Select Security.
Under "Signing in to Google," select App Passwords. You may need to sign in. If you don’t have this option, it might be because:
    2-Step Verification is not set up for your account.
    2-Step Verification is only set up for security keys.
    Your account is through work, school, or other organization.
    You turned on Advanced Protection.
At the bottom, choose Select app and choose the app you using and then Select device and choose the device you’re using and then Generate.
Follow the instructions to enter the App Password. The App Password is the 16-character code in the yellow bar on your device.
Tap Done.
Tip: Most of the time, you’ll only have to enter an App Password once per app or device, so don’t worry about memorizing it.
%}
