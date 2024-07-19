# Besturingssysteem modeltreinen

## Project Structure:
```
/
├── hardware-library/
|   └── interface.rkt
├── infrabel/
|   └── gui/
|   |   ├── gui-infrabel.rkt
|   |   └── gui-startup.rkt
|   ├── main.rkt
|   └── udp.rkt
├── provider/
|   └── gui/
|       └── main.rkt
├── railway/
|   ├── crossing.rkt
|   ├── detection-block.rkt
|   ├── light.rkt
|   ├── main.rkt
|   ├── segment.rkt
|   ├── switch.rkt
|   ├── track.rkt
|   └── train.rkt
├── simulator/
|   └── interface.rkt
├── DB.rkt
├── INFRABEL.rkt
└── NMBS.rkt
```

## Project Control Flow:
### Railway

### Infrabel

### Provider

## Startup
0. Start INFRABEL.rkt, when server online, providers can connect.
0. Connect providers to server (DB.rkt / NMBS.rkt).