package co.com.sura.entity.moviles;

import co.com.sura.entity.agenda.Profesional;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Desplazamiento {
    private String idCitaPartida;
    private String idCitaDestino;
    private LocalDateTime fechaProgramada;
    private Integer idHorarioTurno;
    private String tipo;
    private Integer idEstado;
    private Integer duracion;
    private Integer holgura;
    private String idMovil;

    private Conductor conductor;
    private Profesional profesional;
}
