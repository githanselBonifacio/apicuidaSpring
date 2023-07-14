package co.com.sura.entity.maestro;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class PlanSalud {
    private Integer id;
    private Integer idPlan;
    private String nombre;
    private String nombrePrestador;
}
