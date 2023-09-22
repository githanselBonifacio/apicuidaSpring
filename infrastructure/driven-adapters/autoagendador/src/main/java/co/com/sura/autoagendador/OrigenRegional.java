package co.com.sura.autoagendador;

import lombok.Getter;

@Getter
public enum OrigenRegional {
    BARRANQUILLA (
            new CitaGenetic()
                    .toBuilder()
                    .idCita("sedeBarranquilla")
                    .duracion(0)
                    .holgura(900)
                    .longitud(-74.81236459)
                    .latitud(11.00070515)
                    .build()

    ),
    MEDELLIN (
            new CitaGenetic()
                    .toBuilder()
                    .idCita("sedeMedellin")
                    .duracion(0)
                    .holgura(900)
                    .longitud(-75.573997)
                    .latitud(6.228581)
                    .build()
    ),
    BOGOTA (
            new CitaGenetic()
                    .toBuilder()
                    .idCita("sedeBogota")
                    .duracion(0)
                    .holgura(900)
                    .longitud(-74.11628296)
                    .latitud(4.63242228)
                    .build()
    ),
    RIONEGRO (
            new CitaGenetic()
                    .toBuilder()
                    .idCita("sedeRionegro")
                    .duracion(0)
                    .holgura(900)
                    .longitud(-75.378768)
                    .latitud(6.140225)
                    .build()
    ),
    CALI (
            new CitaGenetic()
                    .toBuilder()
                    .idCita("sedeCali")
                    .duracion(0)
                    .holgura(900)
                    .longitud(-76.5448105)
                    .latitud(3.42423427)
                    .build()
    ),;

    private final CitaGenetic citaGenetic;

    OrigenRegional(CitaGenetic citaGenetic) {
        this.citaGenetic = citaGenetic;
    }
    public static OrigenRegional getOrigenCiudadById(IdRegional idCiudad) {

        switch (idCiudad) {
            case BARRANQUILLA:
                return OrigenRegional.BARRANQUILLA;
            case MEDELLIN:
                return OrigenRegional.MEDELLIN;
            case RIONEGRO:
                return OrigenRegional.RIONEGRO;
            case BOGOTA:
                return OrigenRegional.BOGOTA;
            case CALI:
                return OrigenRegional.CALI;
            default:
                throw new IllegalArgumentException("Invalid idCiudad: " + idCiudad);
        }
    }

}
