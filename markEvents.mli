type markings = {
  has_dom_write : IntSet.t;
  has_nondeterminism : StringSet.t IntMap.t;
}
val calculate_markings : Trace.trace -> markings
