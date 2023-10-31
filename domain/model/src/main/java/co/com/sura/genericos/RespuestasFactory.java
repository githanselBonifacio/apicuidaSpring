package co.com.sura.genericos;

import java.time.LocalDate;

public interface RespuestasFactory {

    static ResultadoActualizacionTurno crearResultadoActualizacionTurno(
            String mensaje, String idprofesional, LocalDate fechaTurno){
        return ResultadoActualizacionTurno
                .builder()
                .idProfesional(idprofesional)
                .mensaje(mensaje)
                .fechaTurno(fechaTurno)
                .build();
    }
}
