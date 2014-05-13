import copy
import logging

def linear_interpolation(p1, t1, p2, t2):
  logging.debug("linear_interpolation: t1: %s t2: %s p1: %s p2: %s", t1, t2, p1, p2)
  def single_point(t):
    logging.debug("single_point: %f t2: %s p2: %s [%s]", t, t2, p2, t < t2)
    assert t <= t2, "%f >= %f" % (t, t2)
    if t < t2: return None
    else: return p2

  def straight_line(slope, t):
    assert t <= t2, "%f > %f" % (t, t2)
    p = [0] * 3
    for i in xrange(3):
      p[i] = p1[i] + slope[i]*(t - t1)
    logging.debug("straight_line: %f t2: %s pos: %s", t, t2, p)
    return p

  assert t1 < t2, "%f > %f" % (t1, t2)
  if t1 is None or t1 == t2: return single_point
  else:
    slope = [0]*3
    for i in xrange(3):
      slope[i] = (p2[i] - p1[i])/(t2 - t1)
    return lambda t: straight_line(slope, t)

class Molecule:
  def __init__(self, type, id, geometric_info):
    self._type = type
    self._id = id
    self._last_time = None
    self._geometric_info = geometric_info
    self._pos = None
    self._interpolation_function = None
    self.graphics = None

  @property
  def geometric_info(self):
    return self._geometric_info

  @property
  def type(self):
    return self._type

  @property
  def id(self):
    return self._id

  def pos(self, time):
    logging.debug("%s time: %s last_snapshot: %s", self, time, self._last_time)
    return self._interpolation_function(time)

  def update_snapshot(self, time, space):
    logging.debug("update_snapshot(%s, %s, %s, %s)", self, time, self._last_time, space)
    assert self._last_time is None or time > self._last_time, "%s < %s" % (time, self._last_time)
    last_pos = None
    if self._pos is not None:
      last_pos = [0] * 3
      for i in xrange(3):
        last_pos[i] = self._pos[i]
    self._interpolation_function = linear_interpolation(last_pos, self._last_time, space, time)
    self._last_time = time
    self._pos = space

  def __repr__(self):
    return "<Mol[type=%s, id=%s]>" % (self._type, self._id)

class Data:
  def __init__(self, csv, sampling_interval=0.001):
    self._sampling_interval = sampling_interval
    time, space = 0.0, [[0,0,0], [0,0,0]]
    with open(csv) as f:
      line = '#'
      while line.startswith('#'):
        line = f.readline() # Header

      data = line.split(',')
      data = [col.strip() for col in data]
      time = float(data[0])
      for i in range(3):
        space[0][i] = float(data[3+i])
        space[1][i] = float(data[3+i])

      for line in f.readlines():
        data = line.split(',')
        data = [col.strip() for col in data]
        time = max(time, float(data[0]))
        for i in range(3):
          space[0][i] = min(float(data[3+i]), space[0][i])
          space[1][i] = max(float(data[3+i]), space[1][i])
    self._time = time
    self._space = space
    self._csv = csv

  @property
  def bounds(self):
    return self._space

  @property
  def snapshots(self):
    with open(self._csv) as f:
      time, prev_time = 0.0, 0.0

      prev_snapshot = {}
      snapshot = {}
      geometries = {}
      for line in f.readlines():
        if line.startswith('#'):
          # #D()::sphere(r = 1.)::cuboid(w = 200., h = 200., d = 200.)::(1. ,2. ,3.)::0.
          geos = line.strip()[1:].split('::')
          geos[0] = geos[0].replace('()', '')
          geometries[geos[0]] = geos[1:]
          continue
        data = line.split(',')
        logging.debug("snapshot: %s", data)
        data = [col.strip() for col in data]
        time = float(data[0])
        space = [0]*3
        for i in range(3):
          space[i] = float(data[3+i])
        if prev_time != time:
          logging.debug("yield: time: %s", prev_time)
          yield prev_time, snapshot
          prev_snapshot = snapshot
          snapshot = {}

        geometric_info = None
        for g in geometries.keys():
          if data[2].startswith(g):
            geometric_info = geometries[g]
            break

        mol = prev_snapshot.get(data[2], Molecule(
          geometric_info=geometric_info,
          type=data[1],
          id=data[2]))
        mol.update_snapshot(time, space)
        snapshot[data[2]] = mol
        prev_time = time
      # The last one
      logging.debug("yield: time: %s", prev_time)
      yield prev_time, snapshot

  def interpolated_snapshots(self):
    time = 0.0
    for snapshot_time, snapshot in self.snapshots:
      while time <= snapshot_time:
        interpolated_snaphot = {}
        for mol in snapshot.values():
          pos = mol.pos(time)
          if pos is None:
            continue
          interpolated_snaphot[mol] = pos
        yield time, interpolated_snaphot
        time += self._sampling_interval

if __name__ == '__main__':
  import sys
  import tempfile
  logging.basicConfig(level=logging.DEBUG)

  t = tempfile.NamedTemporaryFile()
  t.write("""Time,"A()"
  0.,A(),A0,1,0.,0
  0.,B(),B0,1,0.,0
  1,A(),A0,2,0,0
  1,B(),B0,2,0,0
  1,C(),C0,2,0,0
  2,A(),A0,3,0,0
  2,C(),C0,3,0,0
  2,B(),B0,3,0,0 """)
  t.flush()

  d = Data(t.name)
  data = {}
  itr = 0
  for t, snapshot in d.interpolated_snapshots():
    print "* ", t, snapshot
    for m, p in snapshot.iteritems():
      list = data.get(m.id, [])
      list.append(p)
      data[m.id] = list
    if itr == 0:
      assert t == 0.0, t
      assert data.keys() == ['A0', 'B0'], data.keys()
      assert len(data['A0']) == 1
      assert len(data['A0']) == len(data['B0'])
      assert data['A0'] == data['B0']
      rounded_last = [round(f) for f in data['A0'][-1]]
      assert [1.0, 0, 0] == rounded_last, rounded_last
      rounded_first = [round(f) for f in data['A0'][0]]
      assert [1.0, 0, 0] == rounded_first, rounded_first
      data = {}
    elif itr == 1.0/d._sampling_interval:
      assert round(t, 2) == 1.0, t
      assert data.keys() == ['A0', 'C0', 'B0'], data.keys()
      assert len(data['A0']) == 1.0/d._sampling_interval, len(data['A0'])
      assert len(data['A0']) == len(data['B0'])
      assert len(data['C0']) == 1
      assert data['A0'] == data['B0']
      rounded_last = [round(f) for f in data['A0'][-1]]
      assert [2.0, 0, 0] == rounded_last, rounded_last
      rounded_first = [round(f) for f in data['A0'][0]]
      assert [1.0, 0, 0] == rounded_first, rounded_first
      data = {}
    elif itr == 2.0/d._sampling_interval:
      assert round(t, 2) == 2.0, t
      assert data.keys() == ['A0', 'C0', 'B0'], data.keys()
      assert len(data['A0']) == 1.0/d._sampling_interval, len(data['A0'])
      assert len(data['A0']) == len(data['B0'])
      assert data['A0'] == data['B0']
      assert data['A0'] == data['C0']
      rounded_last = [round(f) for f in data['A0'][-1]]
      assert [3.0, 0, 0] == rounded_last, rounded_last
      rounded_first = [round(f) for f in data['A0'][0]]
      assert [2.0, 0, 0] == rounded_first, rounded_first
      data = {}
    itr += 1
