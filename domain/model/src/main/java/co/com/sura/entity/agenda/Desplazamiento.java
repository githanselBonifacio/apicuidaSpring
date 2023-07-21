package co.com.sura.entity.agenda;

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
    private Integer duracion;
    private Integer holgura;
}
