import numpy as np
import pylab as plt

data = open('thinks-its-up.wfm', 'rb').read()[838 + 64:-1]
pts = np.frombuffer(data, dtype=np.int16)
N = 100000
pts = pts.astype(np.float32)
plt.plot(pts[:N] > 0)
pt_min = np.min(pts[:N])
pt_max = np.max(pts[:N])
print(pt_min, pt_max, pt_max - pt_min)
plt.plot((pts[:N] - pt_min) / (pt_max - pt_min))
#plt.hlines(0, 0, len(pts))
plt.vlines([2080 - 200 + 100 + 200*i for i in range(100)], -0.25, 0.25, 'k')
plt.vlines([2080 - 200 + 0 + 200*i*10 for i in range(100)], -0.5, 0.5, 'r')
plt.show()
