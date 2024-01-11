module Util.Ecs.Component exposing (update)

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
