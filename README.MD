# To Run the application

the app is based on .NET 5
Start te API project in the solution from Visual Studio (2019 or 2022)
The browser should open with the swagger interface. All the commands can be tested in there

# Outstanding activities

- Extend Input validation. E.g. negative values, odds <= 1.
- Establish limits for number of markets and orders. IDs are ints so numerically it can manage 2^32 items, however there is no check.
- Protection against memory overflow. Memory consumption is not monitored.
- More tests for edge cases and full coverage. Currently tests cover predominantly successful workflows.
- Add logging. Logging has been left out of this exercise implementation.
- Better exception management (currently exceptions in MailBox manager handler are ignored)