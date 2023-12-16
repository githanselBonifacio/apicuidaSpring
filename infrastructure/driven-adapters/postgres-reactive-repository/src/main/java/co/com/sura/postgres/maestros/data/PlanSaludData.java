package co.com.sura.postgres.maestros.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "plan_salud")
public class PlanSaludData {
    @Id
    private Integer id;
    private Integer idPlan;
    private String nombre;

    @Column(value = "nombre_prestador")
    private String nombrePrestador;
}
