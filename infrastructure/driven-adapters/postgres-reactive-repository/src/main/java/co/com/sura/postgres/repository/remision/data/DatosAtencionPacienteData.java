package co.com.sura.postgres.repository.remision.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "datos_atencion_paciente")
public class DatosAtencionPacienteData {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column("id_datos_atencion")
    private Integer idDatosAtencion;
    @Column("nombre_cuidador")
    private String nombreCuidador;
    @Column("nombre_responsable")
    private String nombreResponsable;
    @Column("telefono_paciente")
    private String telefonoPaciente;
    @Column("celular_paciente")
    private String celularPaciente;
    @Column("celular_paciente2")
    private String celularPaciente2;
    @Column("id_remision")
    private String idRemision;
}
