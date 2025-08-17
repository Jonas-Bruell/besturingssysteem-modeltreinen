# Besturingssysteem modeltreinen

Documentatie:
- [Specificatie voor Ontwikkelaars](docs/specificatie%20voor%20ontwikkelaars/specificatie-voor-ontwikkelaars.pdf)
- [Handleiding voor Eindgebruikers](docs/handleiding%20voor%20eindgebruikers/handleiding-voor-eindgebruikers.pdf)

## Project Structure:
```
/
├── infrabel/
|   ├── logic/
|   ├── logs/
|   ├── gui.rkt
|   ├── interface.rkt
|   ├── server.rkt
|   └── startup.rkt
├── provider/
|   ├── gui-tab-panels/
|   ├── logs/
|   ├── gui.rkt
|   ├── interface.rkt
|   ├── client.rkt
|   └── startup.rkt
├── railway/
|   ├── algorithms/
|   ├── gui-tab-panels/
|   ├── components/
|   |   ├── crossing.rkt
|   |   ├── detection-block.rkt
|   |   ├── light.rkt
|   |   ├── segment.rkt
|   |   ├── switch.rkt
|   |   └── train.rkt
|   └── interface.rkt
├── track/
|   ├── hardware-library/
|   |   └── interface.rkt
|   ├── routes/
|   └── simulator/
|       └── interface.rkt
├── DB.rkt
├── INFRABEL.rkt
└── NMBS.rkt
```

## Startup
1. Start INFRABEL.rkt, when server online, providers can connect.
0. Connect providers to server (DB.rkt / NMBS.rkt).