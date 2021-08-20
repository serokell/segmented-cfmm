import {Get_time_weighted_sum} from  './Get_time_weighted_sum';
import {Increase_observation_count} from  './Increase_observation_count';
import {Set_position} from  './Set_position';
import {X_to_X_prime} from  './X_to_X_prime';
import {X_to_Y} from  './X_to_Y';
import {Y_to_X} from  './Y_to_X';
export type Parameter =
  | Get_time_weighted_sum
  | Increase_observation_count
  | Set_position
  | X_to_X_prime
  | X_to_Y
  | Y_to_X
