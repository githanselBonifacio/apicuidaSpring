package co.com.sura.entity.agenda;

import lombok.Data;
import java.util.List;

@Data
public class ProfesionalWithTurno extends  Profesional{

    private List<TurnoProfesional> turnos;

}
