import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d.axes3d as p3
import matplotlib.axes as axes
import matplotlib.animation as animation

class Object:
  def __init__(self, type, id):
    self.type = type
    self.id = id
    self._pos = None
    self.next_snapshot = None

  def update_snapshot(self, ts, x, y, z):
    self.next_snapshot = (ts, x, y, z)

  def pos(self, ts, fps):
    assert ts <= self.next_snapshot[0], "Time now: %s, next known time: %s" % (ts, self.next_snapshot[0])
    if self._pos is None:
      self._pos = self.next_snapshot[1:]
    else:
      delta = self.next_snapshot[0] - ts
      steps = (fps*delta) + 1
      cx, cy, cz = self._pos
      nx, ny, nz = self.next_snapshot[1:]
      self._pos = cx + (nx - cx)/steps, cy + (ny - cy)/steps, cz + (nz - cz)/steps

    return self._pos

class CsvFile:
  def __init__(self, filename, duration):
    self.time_scale_factor, self.bounds = self._get_bounds(filename)
    self.time_scale_factor = 1 #duration/self.time_scale_factor

    self.file = open(filename)
    self.file.readline() # Header
    self.unprocessed_line = self.file.readline()

  def _get_bounds(self, filename):
    file = open(filename)
    file.readline() # Header
    line = file.readline()
    line_timestamp = None
    mxx, mxy, mxz, mnx, mny, mnz = None, None, None, None, None, None
    while line:
      line_timestamp, species, id, x, y, z = [c.strip() for c in line.split(',')]
      x, y, z = float(x), float(y), float(z)
      if mxx is None:
        mxx, mxy, mxz, mnx, mny, mnz = x, y, z, x, y, z

      mxx, mxy, mxz, mnx, mny, mnz = max(mxx, x), max(mxy, y), max(mxz, z), min(mnx, x), min(mny, y), min(mnz, z)
      line = file.readline()
    return float(line_timestamp), ((mnx, mxx), (mny, mxy), (mnz, mxz))

  def next_snapshot(self, objects):
    line = self.unprocessed_line
    frame_timestamp = None
    new = {}
    while line:
      line_timestamp, species, id, x, y, z = [c.strip() for c in line.split(',')]
      line_timestamp = self.time_scale_factor*float(line_timestamp)
      x, y, z = float(x), float(y), float(z)
      if frame_timestamp is None:
        frame_timestamp = line_timestamp

      self.unprocessed_line = line
      if frame_timestamp != line_timestamp:
        break

      obj = objects.get(id, Object(species, id))
      obj.update_snapshot(frame_timestamp, x, y, z)
      new[id] = obj

      line = self.file.readline()
    return frame_timestamp, new

  def close(self):
    self.file.close()

class Animation:
  def __init__(self, filename, fps=25, duration=30):
    self._datfile = CsvFile(filename, duration)
    self._duration = duration
    self._fps = fps
    self._frame = 0
    self._next_snapshot_ts, self._objs = self._datfile.next_snapshot({})
    self._colors = [
        [1.0, 0.0, 0.0],
        [0.0, 1.0, 0.0], 
        [0.0, 0.0, 1.0],
        [1.0, 1.0, 0.5],
        [0.5, 1.0, 1.0], 
        [0.5, 1.0, 1.0]]
    self._color_map = {}

  def colors(self, id):
    color = self._color_map.get(id)
    if color is None:
      next = len(self._color_map)
      color = self._colors[next]
      self._color_map[id] = color
    return color

  def update(self, num):
    xs, ys, zs, colors = [], [], [], []

    now = self._frame/float(self._fps)
    while now > self._next_snapshot_ts:
      self._next_snapshot_ts, self._objs = self._datfile.next_snapshot(self._objs)

    for o in self._objs.values():
      if now <= self._next_snapshot_ts:
        x, y, z = o.pos(now, self._fps)
        colors.append(self.colors(o.type))
        xs.append(x)
        ys.append(y)
        zs.append(z)
    self._frame += 1
    if now > self._duration:
      return
    if not xs:
      import pdb; pdb.set_trace()
    self._ax.clear()
    self._ax.set_autoscale_on(False)
    self._ax.scatter3D(xs, ys, zs, c=colors)

  def plot(self):
    # Attaching 3D axis to the figure
    fig = plt.figure()
    ax = p3.Axes3D(fig)

    # Setting the axes properties
    ax.set_xlim3d(self._datfile.bounds[0])
    ax.set_xlabel('X')

    ax.set_ylim3d(self._datfile.bounds[1])
    ax.set_ylabel('Y')

    ax.set_zlim3d(self._datfile.bounds[2])
    ax.set_zlabel('Z')

    ax.set_title('3D Plot')
    self._ax = ax

    # Creating the Animation object
    line_ani = animation.FuncAnimation(
        fig,
        self.update)
    plt.show()

if __name__ == '__main__':
  import sys
  a = Animation(sys.argv[1])
  a.plot()
