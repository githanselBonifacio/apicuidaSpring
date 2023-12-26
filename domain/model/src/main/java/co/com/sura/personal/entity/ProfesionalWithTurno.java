package co.com.sura.personal.entity;

import lombok.*;
import lombok.experimental.SuperBuilder;


import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class ProfesionalWithTurno extends Profesional {

    private List<TurnoProfesional> turnos;
}
