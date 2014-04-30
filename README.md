# hpt - Haskell Private talk #

hpt is designed to be a private messaging application that puts the
emphasis on your privacy and security.
To support this claim, here is a list of hpt's features :

- hpt is __open source__ (you can obviously look at the code)
- all transfers are __encrypted__ using SSL (may it be between Alice
  and Bob or Alice and Dispatcher)
- messaging is __peer-to-peer__ : your messages don't transit through
  a potentially _data-stealing_ server (the Dispatcher is only here to
  make contact list available)

hpt is vanilla : this is a messaging application and should be used to
communicate (privately and securely), therefore you won't find fancy,
unecessary and slow features. hpt is supposed to be a messaging
application, it is, period.

## Changelog ##

- April 29, 2014
    - Created self-signed certificate for the dispatcher (for testing
      purpose)
    - Implemented client signing / registering procedures (Born
      request)

- April 25, 2014
    - Creation of the project
    - Defined primary objectives and system architecture
    - Defined types


## TODO ##

- Rewrite functions with Exception handling (e.g. getPrivateKeyFile,
  etc)
- Encrypt the registered.db file in the dispatcher
