package co.com.sura.postgres.repository.agenda.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import java.time.LocalDate;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "profesionales")
public class ProfesionalData {
    @Id
    @Column("numero_identificacion")
    private String numeroIdentificacion;
    @Column("id_tipo_identificacion")
    private Integer idTipoIdentificacion;
    private String nombre;
    private String apellido;
    @Column("fecha_nacimiento")
    private LocalDate fechaNacimiento;

    @Column("id_ciudad")
    private String idCiudad;
    private boolean activo;
}
