package co.com.sura.autoagendador;

import lombok.Getter;

@Getter
public enum OrigenCiudad {
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

    OrigenCiudad(CitaGenetic citaGenetic) {
        this.citaGenetic = citaGenetic;
    }
    public static OrigenCiudad getOrigenCiudadById(IdCiudad idCiudad) {

        switch (idCiudad) {
            case BARRANQUILLA:
                return OrigenCiudad.BARRANQUILLA;
            case MEDELLIN:
                return OrigenCiudad.MEDELLIN;
            case RIONEGRO:
                return OrigenCiudad.RIONEGRO;
            case BOGOTA:
                return OrigenCiudad.BOGOTA;
            case CALI:
                return OrigenCiudad.CALI;
            default:
                throw new IllegalArgumentException("Invalid idCiudad: " + idCiudad);
        }
    }

}
