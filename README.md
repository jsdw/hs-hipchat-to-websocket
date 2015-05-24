# What is it?

An adaptor which makes it easy to connect a websocket based chat bot (using JSON) with Hipchats APIV2. Its primary use is for bypassing the cruft of interfacing with Hipchat. Instead, write your chat bot (or other chat service) in the form of a websocket server using a basic JSON protocol for sending/receiving messages, and let this program do the heavy lifting of talking back and forth with hipchat and being "installable" as a hipchat integration and reauthenticating as necessary.

# Installation

You'll need GHC > 7.10 and a unix based OS (should be fine on linux or OSX). Untested on windows (I have no access/requirement in this direction) though I expect not difficult to get working (perhaps a couple of `withSocketsDo` lines added in the right places).

a simple:

```
cabal sandbox init
cabal install --only-dependencies
cabal build
```

should do the trick, using a sandbox to avoid clashes.

# Usage

The only argument to the produced binary is the location of a config file (by default it'll be `bots.conf`). The config file is pretty simple and looks something like (where lines beginning -- are comments):

```
-- address of this server (must be accessible to hipchat)
address  myaddress.com
-- port for this server
port     10001

-- define bot with id examplebot
-- we can have as many bots defined as we like

[examplebot]

-- name of bot in hipchat
name     Example Bot
-- address of bots websocket server
address  127.0.0.1
-- port for bots websocket server
port     9090
-- message colour (optional, default false)
colour   yellow
-- notify people on message (optional, default false)
notify   false

```

Global settings come first; the address and port of the adapter (which hipchat must be able to see). Next, we define as many bots as we like by using the `[botid]` heading, which should be a unique (to this config file) bot ID, and then some bot details, including the address and port of the, bot's websocket server (which must be reachable from this program), the name of the bot as you'd like it to appear in hipchat, and the colour that its messages will be (one of yellow | green | red | purple | grey | random).

The program starts up an HTTP server on the port given, which you can visit to see basic bot information and a link which you can provide to hipchat in the "add integration by URL" option to install the bot of your choice.

# Bot Servers

The program sends messages to each bot in the config file that is present in the room via websocket connection, taking the form:

```
{
	"room": "SomeRoom",
	"message": "message text",
	"name": "@someone"
}
```

The response from the chat bot is expected to include a room name and message text, which is then relayed on to hipchat. For example:

```
{
	"room": "SomeRoom",
	"message": "response to message"
}
```

Bots can be restarted at will and this program should automatically reconnect them when they reappear (so long as they are on the same address/port!). This program however is stateless and if restarted will not remember the necessary information to continue serving existing bot installations, and so bots will need to be re-installed in hipchat in that event.