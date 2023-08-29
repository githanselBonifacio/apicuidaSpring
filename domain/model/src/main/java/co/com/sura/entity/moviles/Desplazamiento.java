package co.com.sura.entity.moviles;

import co.com.sura.entity.agenda.Profesional;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
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
    private Integer idDesplazamiento;
    private String idCitaPartida;
    private String idCitaDestino;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
    private LocalDateTime fechaProgramada;
    private Integer idHorarioTurno;
    private String tipo;
    private Integer idEstado;
    private Integer duracion;
    private Integer holgura;
    private String idMovil;
    private Profesional profesional;
}
