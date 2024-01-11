module Util.Ecs.Component exposing (set, update)

import Ecs
import Ecs.Component


update : (comp -> comp) -> Ecs.Component.Spec comp world -> Ecs.Entity -> world -> world
update fn spec entity world =
    spec.set
        (world
            |> spec.get
            |> Ecs.Component.update entity fn
        )
        world


set : Ecs.Component.Spec comp world -> Ecs.Entity -> comp -> world -> world
set spec entity comp world =
    spec.set
        (world
            |> spec.get
            |> Ecs.Component.set entity comp
        )
        world
