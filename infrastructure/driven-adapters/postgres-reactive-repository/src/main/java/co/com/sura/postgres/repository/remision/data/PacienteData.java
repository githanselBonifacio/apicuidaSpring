package co.com.sura.postgres.repository.remision.data;

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
@Table(name = "pacientes")
public class PacienteData {
    @Id
    @Column("numero_identificacion")
    private String numeroIdentificacion;

    @Column("tipo_identificacion")
    private String tipoIdentificacion;

    private String nombres;
    private String apellidos;
    private String edad;
    private String sexo;
    private String peso;
    @Column("tipo_afiliacion")
    private String tipoAfiliacion;
    @Column("nombre_aseguradora")
    private String nombreAseguradora;
    @Column("fecha_nacimiento")
    private LocalDate fechaNacimiento;
    @Column("id_ubicacion")
    private String idUbicacion;
}
