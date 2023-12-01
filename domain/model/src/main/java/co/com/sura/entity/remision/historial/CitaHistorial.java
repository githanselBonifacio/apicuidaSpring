package co.com.sura.entity.remision.historial;

import co.com.sura.entity.agenda.Cita;
import co.com.sura.entity.remision.datosremision.Tratamiento;
import co.com.sura.entity.remision.procedimientos.Procedimientos;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.util.List;
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class CitaHistorial extends Cita {

    private List<Tratamiento> tratamientos;
    private Procedimientos procedimientos;

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
