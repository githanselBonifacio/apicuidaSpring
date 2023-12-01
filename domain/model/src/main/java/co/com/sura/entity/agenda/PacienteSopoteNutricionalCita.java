package co.com.sura.entity.agenda;

import co.com.sura.entity.remision.procedimientos.SoporteNutricional;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class PacienteSopoteNutricionalCita {
    private String numeroIdentificacion;
    private String tipoIdentificacion;
    private String nombre;
    private String apellido;
    private SoporteNutricional tratamiento;
    private LocalDate fechaProgramada;
}
