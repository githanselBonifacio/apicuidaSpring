package co.com.sura.personal.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;


import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class ProfesionalWithTurno extends Profesional {

    private List<TurnoProfesional> turnos;
}
