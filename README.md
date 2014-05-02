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

- v0.1.0
    - Communications with dispatcher are SSL/TLS encrypted 
    - Clients can register a new name and connect to dispatcher
    - Clients can add and delete a contact

## TODO ##

- __Encrypt the registered.db file in the dispatcher__
- __Encrypt the user's contact list with his password (it is nobody
  else's business but him to peer into his contact list)__
- __Use SSL/TLS encrypted communications between peers__
- Write a function to remove from alive the users whose last Alive
  request is more than XX time
- Rewrite functions with Exception handling (e.g. getPrivateKeyFile,
  etc)
