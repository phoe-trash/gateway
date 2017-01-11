# Gateway
RP and chat server/client written in Common Lisp

**Dependencies:** `SECURE-READ` library, found in a neighboring repository.

**Current functionality:**
  * See the `/commands/` folder for the commands acceptable from the client.
  * See the `/operations/` folder for operations executable on the server.
  * See the `/protocols/` folder for declared protocols.
  * See the `/classes/` folder for implementation of the protocols.

Don't mind the `/__old/` folder, that's stuff that is being refactored into proper project structure.

To run tests, load the project with ASDF/Quicklisp and `(gateway::run)`.
