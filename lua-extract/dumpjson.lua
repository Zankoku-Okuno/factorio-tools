#!/usr/bin/env lua


data = {
  items = {},
  recipes = {},
  tech = {},
  -- TODO items
  extend = function (self, objs)
      for _, obj in ipairs(objs) do
        if obj.type == "item"
        or obj.type == "item-with-entity-data"
        or obj.type == "tool"
        or obj.type == "repair-tool"
        or obj.type == "ammo"
        or obj.type == "armor"
        or obj.type == "capsule"
        or obj.type == "gun"
        or obj.type == "module"
        or obj.type == "rail-planner" then
          self.items[#self.items+1] = obj
        elseif obj.type == "recipe" then
          if obj.normal == nil then
            obj.normal = {
              enabled = obj.enabled,
              ingredients = obj.ingredients,
              energy_required = obj.energy_required,
              result = obj.result,
              result_count = obj.result_count,
              results = obj.results,
            }
            obj.expensive = obj.normal
            obj.enabled = nil
            obj.ingredients = nil
            obj.energy_required = nil
            obj.result = nil
            obj.result_count = nil
            obj.results = nil
          end
          self.recipes[#self.recipes+1] = obj
        elseif obj.type == "technology" then
          if obj.unit.count ~= nil then
            obj.type = nil
            self.tech[#self.tech+1] = obj
          end
        elseif obj.type == "blueprint-book"
            or obj.type == "upgrade-item"
            or obj.type == "deconstruction-item"
            or obj.type == "selection-tool"
            or obj.type == "item-with-tags"
            or obj.type == "item-with-label"
            or obj.type == "copy-paste-tool"
            or obj.type == "blueprint"
            or obj.type == "item-with-inventory" then
          -- do nothing
        else
          io.stderr:write("factorio extractor: unrecognized type: " .. obj.type .. "\n")
          os.exit()
        end
      end
    end,
}


require "prototypes.technology.inserter"
require "prototypes.technology.military-upgrades"
require "prototypes.technology.technology"

require "prototypes.recipe.ammo"
require "prototypes.recipe.capsule"
require "prototypes.recipe.demo-recipe"
require "prototypes.recipe.equipment"
require "prototypes.recipe.fluid-recipe"
require "prototypes.recipe.inserter"
require "prototypes.recipe.module"
require "prototypes.recipe.recipe"
require "prototypes.recipe.turret"

require "prototypes.item.ammo"
require "prototypes.item.armor"
require "prototypes.item.capsule"
require "prototypes.item.demo-ammo"
require "prototypes.item.demo-armor"
require "prototypes.item.demo-gun"
require "prototypes.item.demo-item"
require "prototypes.item.demo-turret"
require "prototypes.item.equipment"
require "prototypes.item.gun"
require "prototypes.item.item"
require "prototypes.item.module"
require "prototypes.item.turret"


luna = require "lunajson"
-- print(luna.encode(data.tech[140]))
print(luna.encode(
  { items = data.items
  , recipes = data.recipes
  , technology = data.tech
  }))
os.exit()



-- science name shortener
packnicks = {
  ["automation-science-pack"] = "red",
  ["logistic-science-pack"] = "green",
  ["military-science-pack"] = "black",
  ["chemical-science-pack"] = "blue",
  ["production-science-pack"] = "purple",
  ["utility-science-pack"] = "yellow",
  ["space-science-pack"] = "white",
}
function to_nicks(codenames)
  out = {}
  for i, codename in ipairs(codenames) do
    out[i] = packnicks[codename[1]]
  end
  return out
end

-- summarize all data into relevant data
-- initialize dependency graph
tab = {}
graph = {}
for i, t in ipairs(tech) do
  -- io.stderr:write(luna.encode(t.name, t.unit.count) .. "\n")
  if t.unit.count ~= nil then
    tab[t.name] = {
      packs = to_nicks(t.unit.ingredients),
      count = t.unit.count,
      ["lab-minutes"] = t.unit.count * t.unit.time / 60,
      ["science per lab-minute"] = t.unit.time / 60,
    }
    graph[t.name] = {}
    if t.prerequisites ~= nil
    then
      for _, pre in ipairs(t.prerequisites) do
        graph[t.name][pre] = true
      end
    end
  end
end

-- find the transitive closure of the tech prerequisites
fixed = true
while true do
  for from, directPrereqs in pairs(graph) do
    for to, _ in pairs(directPrereqs) do
      transPrereqs = graph[to]
      for trans, _ in pairs(transPrereqs) do
        if graph[from][trans] ~= true then
          graph[from][trans] = true
          fixed = false
        end
      end
    end
  end
  if fixed then break else fixed = true end
end

-- attach transitive dependencies to tech table
for tname, tinfo in pairs(tab) do
  tinfo.deps = graph[tname]
end

-- output json
dump = luna.encode(tab)
print(dump)