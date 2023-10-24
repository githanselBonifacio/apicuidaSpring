package co.com.sura.entity.agenda;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class TurnoProfesional {
    @JsonIgnore
    private String idTurno;
    private LocalDate fechaTurno;
    private Integer idHorarioTurno;
    private String idProfesional;
    private String idRegional;
}
