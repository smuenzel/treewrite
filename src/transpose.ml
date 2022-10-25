open! Core

let map2 (type ka kb v ca cb)
    ~empty
    (x : (ka, (kb,v,cb) Map.t, ca) Map.t) 
  : (kb, (ka,v,ca) Map.t, cb) Map.t
  =
  let comparator_outer = Map.comparator_s x in
  let comparator_inner = Map.comparator_s empty in
  Map.fold x
    ~init:(Map.empty comparator_inner)
    ~f:(fun ~key:key_outer ~data acc ->
        Map.fold data ~init:acc ~f:(fun ~key:key_inner ~data acc ->
            let new_map =
              match Map.find acc key_inner with
              | None -> Map.singleton comparator_outer key_outer data
              | Some map -> Map.add_exn map ~key:key_outer ~data
            in
            Map.set acc ~key:key_inner ~data:new_map
          )
      )

type (_,_) sel3 =
  | S1 : (('a * _ * _), 'a) sel3
  | S2 : ((_ * 'a * _), 'a) sel3
  | S3 : ((_ * _ * 'a), 'a) sel3

let sel3_fst
    (type a b c r x0 x1 x2 r')
    (selector : ((a * x0) * (b * x1) * (c * x2), (r * r')) sel3)
    ((a,b,c) : (a * b * c))
  : r
  =
  match selector with
  | S1 -> a
  | S2 -> b
  | S3 -> c

let sel3_cmp
    (type a b c r a' b' c' r')
    (selector : ((a * a') * (b * b') * (c * c'), (r * r')) sel3)
    ((a,b,c) :
       ( (a, a') Comparator.Module.t
         * (b, b') Comparator.Module.t
         * (c, c') Comparator.Module.t
       ))
  : (r, r') Comparator.Module.t
  =
  match selector with
  | S1 -> a
  | S2 -> b
  | S3 -> c

let tranpose3_gen
    (type k1 k1' k2 k2' k3 k3' v c1 c1' c2 c2' c3 c3')
    (selector :
       (
         ((k1 * c1) * (k2 * c2) * (k3 * c3), (k1' * c1')) sel3
         * ((k1 * c1) * (k2 * c2) * (k3 * c3), (k2' * c2')) sel3
         * ((k1 * c1) * (k2 * c2) * (k3 * c3), (k3' * c3')) sel3
       )
    )
    ~empty2
    ~empty3
    (x : (k1, (k2,(k3,v,c3) Map.t,c2) Map.t, c1) Map.t)
  : (k1', (k2',(k3',v,c3') Map.t,c2') Map.t, c1') Map.t
  =
  let s1, s2, s3 = selector in
  let comparator_i3 = Map.comparator_s empty3 in
  let comparator_i2 = Map.comparator_s empty2 in
  let comparator_i1 = Map.comparator_s x in
  let c = comparator_i1, comparator_i2, comparator_i3 in
  let comparator_o3 = sel3_cmp s3 c in
  let comparator_o2 = sel3_cmp s2 c in
  let comparator_o1 = sel3_cmp s1 c in
  Map.fold x
    ~init:(Map.empty comparator_o1)
    ~f:(fun ~key:key_i1 ~data acc ->
        Map.fold data ~init:acc ~f:(fun ~key:key_i2 ~data acc ->
            Map.fold data ~init:acc ~f:(fun ~key:key_i3 ~data acc ->
                let keys = key_i1, key_i2, key_i3 in
                let key_o1 = sel3_fst s1 keys in
                let key_o2 = sel3_fst s2 keys in
                let key_o3 = sel3_fst s3 keys in
                let new_map =
                  match Map.find acc key_o1 with
                  | None ->
                    let data = Map.singleton comparator_o3 key_o3 data in
                    Map.singleton comparator_o2 key_o2 data
                  | Some map ->
                    let new_map =
                      match Map.find map key_o2 with
                      | None -> Map.singleton comparator_o3 key_o3 data
                      | Some map -> Map.add_exn map ~key:key_o3 ~data
                    in
                    Map.set map ~key:key_o2 ~data:new_map
                in
                Map.set acc ~key:key_o1 ~data:new_map
              )
          )
      )

let transpose_312 ~empty2 ~empty3 x = tranpose3_gen (S3,S1,S2) ~empty2 ~empty3 x
let transpose_231 ~empty2 ~empty3 x = tranpose3_gen (S2,S3,S1) ~empty2 ~empty3 x

