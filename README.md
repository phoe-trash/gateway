# Gateway
RP and chat server/client written in Common Lisp

**Imzy community:** https://www.imzy.com/gateway

**Dependencies:** `SECURE-READ` library, found in a neighboring repository.

**Current functionality:**
  * See the `/commands/` folder for the commands acceptable from the client.
  * See the `/operations/` folder for operations executable on the server.
  * See the `/protocols/` folder for declared protocols.
  * See the `/classes/` folder for implementation of the protocols.
  * See the `/errors/` folder for the declared errors that may occur during program execution.
  * See the `/conditions/` folder for the declared conditions that may occur during program executions but are not fatal and allow execution to continue.
  * See the `/framework/` folder for the messy, *most* messy stuff.
  * Don't mind the `/__old/` folder, that's old stuff that is being refactored into proper project structure.

No symbols are exported yet because none should be, as this project is still heavily in development.

To run tests, load the project with ASDF/Quicklisp and `(gateway::run)`.
