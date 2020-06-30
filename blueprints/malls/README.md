# Okuno's Factorio Blueprints

## Mall

I want to store all the products of the mall in one place.
For this, I decided to route outputs onto sushi belts and control factory output with a simple circuit network.
The inserters onto the sushi belts are mediated by an inserter which is only enabled if that item has less than zero on the red network.

I also want a mall built around graceful degradation: it should function even at relatively low tech levels
This implies a mall built primarily with belts rather than bots, which poses a nice design challenge.

### Storage

The storage should be composed of chests, loaded by filter inserters, one for each item.
These chests should be wired together to obtain an inventory of all items stored in the mall.
These chests should also be wired to the output-mediation inserters.
To control your inventory, add constant combinators to the network.
However many you want of a given item, set a combinator to output _negative_ that amount.

The items currently stored in chests is positive, which add with the negative request numbers.
Since the output-mediating inserters deactivate once this number is non-negative, they will stop when the items in the chests cancel out the negative numbers you requested.
If there is a delay (and there will be some delay) between when the item is placed on the sushi belt and when it is picked up by storage, the mall may over-produce.
As long as your delay is not excessive, or your requests too near a full chest, this should not be a problem in practice.

An alternative (but not currently blueprinted) method would be to use a memory cell to track the items as the enter and exit the sushi.
This has some downsides however; one is that the onto-sushi pulses need to be kept separate from the output-mediation network.
More annoying however, is the unreliability: if items are removed from the sushi belts before they make it to storage (by players or biters), this can cause the memory cell to become out-of-sync with the real items stored.
There are probably solutions, and they are probably to be discovered by people more familiar with blood buses than I.


### Automation

The design goal was to build all of the belt and inserter technologies in an expandable way.
Each product has one assembler devoted to it, and they are all arranged in a nice grid.
A tile pattern is provided underneath to provide minimum spacing information, but is also A E S T H E T I C.

The design is organized around an input bus, with three sushi belts coming out at 90°, one for each tier of product.
All belts (incl. undergrounds and splitters) are built on one side, and inserters on the other.
Some intermediate products (gears and green circuits) are built up near the input end of the bus to reduce off-site construction in the early game, but additional intermediate products are expected to be added later.
The bus itself has been kept clear of walking obstructions for maintenance or pass-through purposes;
    due to some inserters and power poles, I don't recommend driving through, however.
Lights are included to cover the built-up portion of the module at any time.

The first blueprint as iron and copper plate input on a single belt in their own lane.
As the blueprint progresses, you add two belts of gears (esp. for red/blue undergrounds), a belt of green circuits, a lubricant pipe, a belt of green circuits (esp. for the stack inserters), and a lane of red circuits, and finally an extra lane of iron (to maximize throughput of yellow belt tech which higher tiers depend on).


As with everything else in the mall, this runs intermittently, so I'm not too worried about squeezing full throughput out of it.
However, the end-product should nevertheless strive to avoid bottlenecks as much as possible within the allotted footprint.

#### Too Small

Even before yellow belts and yellow/red inserters, there's a blueprint for gears and green circuits.
Simply add chests at the obvious spots, but remember to remove them before going on to later prints.
If you're using my multi-tool blueprints, I wouldn't recommend this one; instead move on to Tier One.

#### Tier One

This tier constructs yellow belts, undergrounds, and splitters.

It uses one lane of iron and copper plates each, sharing a yellow belt.

#### Tier Two

Here you face some options.
You can lay down either the tier two filters (fast and filter) or red belts.
Due to the gear cost of red undergrounds, I expect the tier two filters will be placed first.

Adding the red belts will require an additional yellow belt of gears.

### Tier Three

Again, we can choose to place down either the belt side (blue) or the filter side (stack) first.
Further, you can even place down the tier three filter side without having placed red belts.

Adding the stack inserters will require an additional red belt of gears, a red belt of green circuits (these bad boys are hungry!), as well as a yellow lane of red circuits.
Adding the blue belts will require a pipe of lubricant, as well as the green and red circuits if you haven't already added them.


#### Final Form

This version adds a number of features to increase throughput and smooth out brief fluctuations.
Assemblers, inserters, and belts are upgraded; buffer chests are added; an additional lane of iron input is added to support the demand for green circuits from splitters; speed and productivity modules are added.
Any of these upgrades can be manually performed earlier or partially as-needed.
However, these upgrades were not integrated into the earlier blueprints to ease re-stamping.

This blueprint cannot be stamped down directly.
This is due to upgrades to assemblers in inserter and belt throughput, the addition of extra buffer chests, and a small change in belt routing to supply extra green circuits.
The procedure is as follows:

  * Upgrade some inserters to stack inserters: all iron/gear inputs to belts, and the green circuit inputs to green and white inserters (eight in total).
  * Delete the extra gear belts after they branch off from the bus.
  * Delete the iron belt as it passes by the yellow underground assembler.
  * Delete the above-ground green circuit belts that pass to the side of the filter-side gear assembler.
  * Upgrade all yellow inserters to blue inserters.
  * Upgrade all belts/undergrounds/splitters to blue.
  * Upgrade all assemblers to tier 3.
  * Delete all small power poles.
  * Stamp the blueprint.
  * Delete the red inserter from the middle of the bus near its start.
  * Add modules as directed or tune as you see fit.

Here's an upgrade planner string:

```
0eNq11G1rgzAQB/Dvcq8Vqg5d/SpjlHS9uoB54O46WiTffcmm0BeCKetemYT8+d2p3AQXP5A64cGPylok6CdgFNF24LQ2ynukuHyb4EzOpDO5eYQe0IqWGxRglUl7xYzmOMZkadTHp7ZYVhAKEPdYqEkhbU94hX4Xik1XW0aSWPqWdVYs5f3tGakzkJU6678295Lh/tQspCx7R1IecZRNFq+ekHklNsOvGfCzzX1us5d4nwZy8fkYvRac8SrnE/8L3OS2zX7UkvMXL+p9YNHaDO0pUBfe007QpBf3O0LKZYQU8BVHhnYW+q7ZVfu2rpquDeEbOueB8g==
```

and a deconstruction planner for the power poles:

```
0eNplj90KwjAMhd8l1x1sDjbsq4iU0mZSzNLRRnGMvrvd0AvxLnwnnJ8NPLrIWdLDSYhsFrLMmEBvkFEk8C3vN7IEWc0USDCZOXoE3apfXB8vG7CdqwZ5tkQNEjpJwTVLJAQFgT2+QHflqkASYjaWvUnR3bOJTCvoyVLGKgbCv7QD5sNzr/rhpdoKzjXzd0rznaLgWctVAnrs2+48nLp+HEp5AwLWWqk=
```

I've added a number of buffer chest to store up an excess supply of high-demand items
    (read: "Gears! Oh my stars the gears!").
These are tuned to keep enough input for a stack's worth of output.
Unfortunately, I couldn't retrofit them into the design for blue splitters.
I also didn't bother with the red transport belts, as a stack inserter pulling from a blue belt should be able to keep up.

I've not upgraded the filter inserter's green circuit input to a stack inserter for ease of upgrade.
Filter inserters I find I use less commonly, so the bottleneck doesn't bother me.
Similarly, there are some stack inserters which may be overkill, but again for ease of upgrading I've upgraded them as well.

#### Further Work

There's space (if you leave it alone) underneath the filter-side gear assembler to place a roboport that covers the area.
At this point, then output chests and circuit network could be replaced with bots if desired.
