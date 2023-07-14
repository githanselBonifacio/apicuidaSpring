package co.com.sura.dto.remision;

import co.com.sura.entity.maestro.TipoIdentificacion;
import co.com.sura.entity.remision.Ubicacion;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class PacienteRequest {
    private TipoIdentificacionRequest tipoIdentificacion;
    private String numeroIdentificacion;
    private String nombre;
    private String apellido;
    private LocalDate fechaNacimiento;
    private UbicacionRequest ubicacion;
}
