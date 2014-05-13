import sys
from data import Data
from visual import *

# Change these...
frame_per_second=10
# Data in the csv is interpolated at this sampling. For data that looks like:
# 0, 1...
# 1, 2..
# For a sampling interval for 0.1 the data will appear like:
# 0.0, 1.0..
# 0.1, 1.1..
# 0.2, 1.2..
# ..
# 1.0, 2.0..
# The samples between the snapshots are linearly interpolated
sampling_interval=0.001
# End

import logging
win=800
radius=5

colors = [color.red, color.green, color.blue,
          color.yellow, color.cyan, color.magenta,
          color.white, color.gray]

data = Data(sys.argv[1], sampling_interval=sampling_interval)
bounds=[b*1.0 for b in data.bounds[1]]
center=[b*0.5 for b in data.bounds[1]]
scene = display(title="Simulation", width=win, center=center, forward=(0,1,-0.5), height=win, range=bounds)

L=100
xaxis = curve(pos=[(0,0,0), (L,0,0)], color=(0.5,0.,0.))
yaxis = curve(pos=[(0,0,0), (0,L,0)], color=(0.,0.5,0.))
zaxis = curve(pos=[(0,0,0), (0,0,L)], color=(0.,0.,0.5))

def wirecube (s):
  c=curve (color=color.white, radius=1)
  pts = [
      (0, 0, 0), (0, 0, s), (0, s, s),
      (0, s, 0), (0, 0, 0), (s, 0, 0),
      (s, s, 0), (0, s, 0), (s, s, 0),
      (s, s, s), (0, s, s), (s, s, s),
      (s, 0, s), (0, 0, s), (s, 0, s),
      (s, 0, 0)]
  for pt in pts:
    c.append(pos=pt)

wirecube(bounds[0])
types={}
prev_snapshot={}
for t, snapshot in data.interpolated_snapshots():
  rate(frame_per_second)

  prev_keys = set(prev_snapshot.keys())
  keys = set(snapshot.keys())

  missing_now = prev_keys - keys
  for k in missing_now:
    k.graphics.visible = False
  prev_snapshot = snapshot

  for m, pos in snapshot.iteritems():
    if m.graphics is None:
      colorcode = types.get(m.type)
      if colorcode is None:
        colorcode = len(types)
        label(
            text=m.type,
            pos=(0, 0, 20*colorcode),
            opacity=0.8,
            color=colors[colorcode])
      types[m.type] = colorcode
      radius = float(m.geometric_info[0].split()[1])
      m.graphics = sphere(radius=radius,
          color=colors[colorcode])
    m.graphics.visible = True
    m.graphics.pos = pos
