do local _=
  { entities=
    { { name="express-transport-belt"
      , position=
        { x=-1
        , y=-2
        }
      , direction=2
      }
    , { name="constant-combinator"
      , position={ x=-1, y=-1 }
      , direction=2
      , control_behavior=
        { filters=
          { { signal=
              { type="virtual"
              , name="signal-red"
              }
            , count=1
            , index=1
            }
          }
          , is_on=false
        }
      , connections=
        { ["1"]=
          { red=
            { { entity_id=5, circuit_id=1 }
            }
          , green=
            { { entity_id=8, circuit_id=1 }
            }
          }
        }
      }
    , { name="express-transport-belt"
      , position={ x=0, y=-2 }
      , direction=2
      , control_behavior=
        { circuit_enable_disable=false
        , circuit_read_hand_contents=true
        , circuit_contents_read_mode=0
        }
      , connections=
        { ["1"]=
          { red=
            { { entity_id=5, circuit_id=1 }
            }
          }
        }
      }
    , { name="express-transport-belt"
      , position={ x=1, y=-2 }
      , direction=2
      }
    , { name="decider-combinator"
      , position={ x=0.5, y=-1 }
      , direction=2
      , control_behavior=
        { decider_conditions=
          { first_signal={ type="virtual", name="signal-red" }
          , constant=610
          , comparator="<"
          , output_signal={ type="virtual", name="signal-everything" }
          , copy_count_from_input=true
          }
        }
      , connections=
        { ["1"]=
          { red=
            { { entity_id=2 }
            , { entity_id=5, circuit_id=2 }
            , { entity_id=3 }
            , { entity_id=9, circuit_id=1 }
            }
          }
        , ["2"]=
          { red=
            { { entity_id=5, circuit_id=1 }
            }
          }
        }
      }
    , { name="constant-combinator"
      , position={ x=-1, y=0 }
      , direction=2
      , control_behavior=
        { filters=
          { { signal=
              { type="virtual"
              , name="signal-red"
              }
            , count=-610
            , index=1
            }
          }
        }
      , connections={ ["1"]={ green={ { entity_id=9, circuit_id=1 } } } }
      }
    , { name="small-lamp"
      , position={ x=-1, y=1 }
      , control_behavior=
        { circuit_condition=
          { first_signal={ type="virtual", name="signal-red" }
          , constant=0
          , comparator=">"
          }
        }
      , connections=
        { ["1"]=
          { green=
            { { entity_id=8, circuit_id=1 }
            }
          }
        }
      }
    , { name="decider-combinator"
      , position={ x=0.5, y=1 }
      , direction=2
      , control_behavior=
        { decider_conditions=
            { first_signal={ type="virtual", name="signal-red" }
            , constant=610
            , comparator="<"
            , output_signal={ type="virtual", name="signal-everything" }
            , copy_count_from_input=true
            }
        }
      , connections=
        { ["1"]=
          { red=
            { { entity_id=8, circuit_id=2 }
            }
          , green=
            { { entity_id=2 }
            , { entity_id=7 }
            }
          }
        , ["2"]=
          { red=
            { { entity_id=8, circuit_id=1 }
            , { entity_id=9, circuit_id=2 }
            }
          }
        }
      }
    , { name="decider-combinator"
      , position={ x=0.5, y=0 }
      , direction=2
      , control_behavior=
        { decider_conditions=
          { first_signal={ type="virtual", name="signal-red" }
          , constant=0
          , comparator="="
          , output_signal={ type="virtual", name="signal-everything" }
          , copy_count_from_input=true
          }
        }
      , connections=
        { ["1"]=
          { red=
            { { entity_id=5, circuit_id=1 }
            }
          , green=
            { { entity_id=6 }
            }
          }
        , ["2"]=
          { red=
            { { entity_id=8, circuit_id=2 }
            }
          }
        }
      }
    }
    , icons=
      { 
        { signal={ type="item", name="decider-combinator" }
        , index=1
        }
      , 
        { signal={ type="item", name="express-transport-belt" }
        , index=2
        }
      }
    };
return _;
end
