I plan to run a train network with 2-6 LHD trains, where station names are shared to support dynamic dispatch of trains.
Mining outposts are designed to be pulled from (and pushed to).
Train stations are designed to be modular to allow for flexibility and experimentation.
I am avoiding bot-based stations so that these blueprints can be used at low technology levels.

## Receiving Center


## Terminus

I say terminus because trains enter an exit on the same side.
I say sequential because it has one station for loading/unloading cargo and fluids.
Features are provided individually in multiple overlaid blueprints, and can support up to passenger, product, and propulsion.

In my rail network, all passenger stations have the same name except for a "Grand Central" station.
However, these stations are disabled by default.
Briefly toggling a nearby power switch (from map view), will enable the station, which should call an appropriately-setup passenger train.
The station is then disabled again once the train arrives.
The passenger station can be set up from external power only, or with on-site solar power.
This solar is enough to power a radar, the calling mechanism, and a little bit more, but if its accumulators fall below 90%, it will save its power so that the radar and calling mechanism continue to function.

The everyday use of the station is more likely for loading/unloading.
For each car, you can choose cargo/fluid, loaded/unloaded, and technology level.
Loading and unloading are balanced, buffered, and filtered.
Working with cargo wagons requires balancing, which is provided by a self-configuring MadZuri smart inserter array.
When (as is likely) more than one car is to be loaded with (unloaded of) the same item, there are two variants to use:
    the one nearest the station should be the "Head" variant,
    and all the others should be the "Tail", and come immediately after the head.
These restrictions are because these blueprints are designed to connect on their own.
The head variant also contains an unconfigured constant combinator which, when set, whitelists the filter inserters onto (out of) the cargo wagon.
Strictly speaking, a single wagon could even be used for both loading and unloading, but the highest-throughput loaders/unloaders do not work together in this way.
Up to twelve belts and pipes can be loaded or unloaded from a single train.
The contents of all chests and fluid tanks is hooked up to a product-station-wide network which can be used to control the station enable/disable; I have not set up a default here, as this depends heavily on the nature of your network and the station's role within it.

An additional fuel dropoff station is added which expects a 1-1-1 refueler train.
Fuel is unloaded from there into buffer chests, and the station only turns on when those buffers are empty.
The fuel is transported to the product and passenger locomotives by belt, where sufficient chest space is configured to supply an empty train.
An additional chest is provided that can be used to manually fill the fuel belt with priority over the fuel train station.
A constant combinator whitelists the types of fuel that can be unloaded; it is configured for any fuel other than wood by default.

The passenger train station is also optionally used for forgetfulness/garbage stations.
If the passenger train contains additional cargo wagons, filter inserters can be configured to retrieve a set number of items into a buffer chest.
These can be configured by setting the negative desired number of items in the nearby constant combinator.
Bollards are placed around these chests to prevent damage when driving by.
If you also have a car for garbage, one (or more) of the forgetfulness chests can be replaced with chests that load the train quickly.
Proper configuration of your passenger train can allow every for outpost to be supplied with commonly-needed or commonly-forgotten items and for excess items to be recycled all automatically.
